package cardinality.compiled

import cardinality.analysis.BuiltinTypes
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.TopTypes
import cardinality.capacity.{BaseTypes, NothingSize, Size, UnitSize}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Modifiers.{TermSymbolKind, Visibility}
import tastyquery.Names.nme
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

/** What the compiled front end measures so far, read from symbols and types
  * (docs/plans/compiled-own-types.md, "Front end: design and status", lists the slices): a `Left`
  * names what a row needs that no slice reads yet, and is never a count.
  *
  * Method rows: a method or value whose owners are packages and objects without parents, so its
  * environment is its own parameters (a concrete member around it computes from what the
  * environment already holds, and an object has no abstract one), over the types the standard model
  * reads and the method's own unbounded type parameters.
  *
  * Definition rows: an object; a class without type parameters, parents or a restricted constructor
  * whose fields are base types; an enum whose cases are all values.
  */
final private[compiled] class Readings(own: Set[ClassSymbol])(using Context) {
  import Readings.Unread
  import Unread.Uncovered

  // ---- method rows --------------------------------------------------------------------------

  /** The count of a method row. */
  def method(tree: ValOrDefDef): Either[String, Count] =
    for {
      _ <- deferred(tree.symbol)
      _ <- static(tree.symbol.owner)
      _ <- plain(tree.symbol.owner)
      count <-
        // No call of a member of an object that is never built returns: one implementation.
        if (!built(tree.symbol.owner, tree)) Right(Count.Finite(1))
        else
          counted(tree) match {
            case Right(count)                 => Right(count)
            case Left(Unread.Refused(reason)) => Right(Count.Unresolved(List(reason)))
            case Left(Uncovered(what))        => Left(what)
          }
    } yield count

  // A goal with one value has one implementation whatever the environment holds (v1.md §2), so
  // the result is read first; else the count is the solver's over the parameters.
  private def counted(tree: ValOrDefDef): Either[Unread, Count] = {
    val (binders, parameters, declared) = tree match {
      case v: ValDef => (Nil, Nil, v.tpt)
      case d: DefDef =>
        (
          d.paramLists.flatMap(_.toOption.toList.flatten),
          d.paramLists.flatMap(_.left.toOption.toList.flatten),
          d.resultTpt
        )
    }
    val self = tree.symbol.displayFullName
    val atoms = binders.map(binder => (binder.symbol: Symbol) -> s"$self[${binder.name}]").toMap
    for {
      _ <- Traversals.sequence(binders.map(unbounded))
      result <- shape(declared.toType, atoms)
      count <-
        if (TopTypes.terminalGoal(TopTypes.polarized(result, true))) Right(Count.Finite(1))
        else
          for {
            _ <- Traversals.sequence(parameters.map(p => deferred(p.symbol))).left.map(Uncovered(_))
            inputs <- Traversals.sequence(parameters.map(p => shape(p.tpt.toType, atoms)))
          } yield Inhabitation.count(
            parameters.lazyZip(inputs).map { (p, read) =>
              Binding(s"$self.${p.name}", read, Some(p.name.toString))
            },
            result
          )
    } yield count
  }

  // What a member's or a parameter's flags ask of slices not read yet.
  private def deferred(symbol: TermSymbol): Either[String, Unit] =
    if (symbol.isInline) Left("an inline definition")
    else if (symbol.isGivenOrUsing || symbol.isImplicit) Left("a contextual definition")
    else if (symbol.isExtensionMethod) Left("an extension method")
    else if (symbol.name == nme.Constructor) Left("a constructor")
    else Right(())

  // Every owner up to the package is an object that inherits nothing.
  private def static(owner: Symbol): Either[String, Unit] = owner match {
    case _: PackageSymbol                  => Right(())
    case c: ClassSymbol if c.isModuleClass =>
      // An object's self type is its own singleton.
      if (c.parents.forall(memberless)) static(c.owner)
      else Left(s"an enclosing object with parents: ${c.name}")
    case c: ClassSymbol => Left(s"an enclosing class: ${c.name}")
    case other          => Left(s"an enclosing definition: ${other.name}")
  }

  // A pattern or multi-name value definition is one statement of the source and several values of
  // the tree: `val a, b = …` a value per name, all starting where the definition does; `val (a, b)
  // = …` a value per name, each positioned at its name alone, with a holder the compiler made or
  // none; `val Some(x) = e` a match on `e` ascribed. Which of them the constructor evaluates, and
  // which is the measured choice, is the body reading's to say (slice 5d).
  private def plain(owner: Symbol): Either[String, Unit] = {
    val values = owner.tree.toList.flatMap {
      case c: ClassDef => c.rhs.body.collect { case v: ValDef if !v.symbol.isModuleVal => v }
      case _           => Nil
    }
    val placed = values.filterNot(_.pos.isUnknown)
    val destructured = values.exists { v =>
      v.symbol.isSynthetic || v.rhs.exists {
        case Match(_: Typed, _) => true
        case _                  => false
      }
    }
    if (
      destructured || placed.exists(v => v.pos.startOffset == v.pos.pointOffset) ||
      placed.groupBy(_.pos.startOffset).exists(_._2.size > 1)
    ) Left("an enclosing object with a pattern or multi-name value definition")
    else Right(())
  }

  // Whether the object a member runs in is ever built (register D26): its constructor evaluates
  // each strict value's initializer and each statement, and one that only throws leaves no
  // instance. The measured member's own initializer is the choice being counted.
  private def built(owner: Symbol, measured: Tree): Boolean =
    owner.tree.forall {
      case c: ClassDef =>
        c.rhs.body.forall {
          case stat if stat eq measured => true
          case v: ValDef      => v.symbol.kind == TermSymbolKind.LazyVal || !v.rhs.exists(throws)
          case term: TermTree => !throws(term)
          case _              => true
        }
      case _ => true
    }

  // Whether evaluating a term always throws: `throw`, the library's `???` and `sys.error`, a block
  // whose result throws, a conditional or match every branch of which throws.
  private def throws(term: TermTree): Boolean = term match {
    case _: Throw                         => true
    case ref: TermReferenceTree           => Readings.member(ref.symbol, "scala", "Predef$", "???")
    case Apply(fun: TermReferenceTree, _) =>
      Readings.member(fun.symbol, "scala.sys", "package$", "error")
    case Block(_, result) => throws(result)
    case If(_, yes, no)   => throws(yes) && throws(no)
    case Match(_, cases)  => cases.nonEmpty && cases.forall(c => throws(c.body))
    case _                => false
  }

  // A parent that declares no member a count reads (v1.md §2).
  private def memberless(parent: Type): Boolean =
    classNamed(parent.dealias).flatMap(Readings.topLevel).exists(Readings.Memberless)

  // The class a type names outright.
  private def classNamed(tpe: Type): Option[ClassSymbol] = tpe match {
    case ref: TypeRef => ref.optSymbol.collect { case c: ClassSymbol => c }
    case _            => None
  }

  // A proper type parameter any type instantiates.
  private def unbounded(binder: TypeParam): Either[Unread, Unit] = {
    val bounds = binder.symbol.declaredBounds
    if (bounds.low.isSameType(defn.NothingType) && bounds.high.isSameType(defn.AnyType)) Right(())
    else Left(Uncovered(s"a bounded or higher-kinded type parameter: ${binder.name}"))
  }

  // A type as the method side reads it: a class the standard model holds, under its own fully
  // qualified name (the compiler bound it, decision D3), or one of `atoms`, the measured method's
  // type parameters with the atom each is.
  private def shape(tpe: Type, atoms: Map[Symbol, String]): Either[Unread, Shape] =
    tpe.dealias match {
      case by: ByNameType         => shape(by.resultType, atoms)
      case repeated: RepeatedType => shape(repeated.elemType, atoms).map(Shape.Repeated(_))
      case ref: TypeRef           => named(ref, Nil, atoms)
      case applied: AppliedType   =>
        applied.tycon match {
          case ref: TypeRef =>
            Traversals
              .sequence(applied.args.map {
                case argument: Type => shape(argument, atoms)
                case _              => Left(Uncovered("a wildcard type argument"))
              })
              .flatMap(named(ref, _, atoms))
          case _ => Left(Uncovered(s"an applied type: ${tpe.showBasic}"))
        }
      case other => Left(Uncovered(s"a type: ${other.showBasic}"))
    }

  private def named(
      ref: TypeRef,
      arguments: List[Shape],
      atoms: Map[Symbol, String]
  ): Either[Unread, Shape] =
    ref.optSymbol match {
      case Some(binder) if atoms.contains(binder) && arguments.isEmpty =>
        Right(Shape.Atom(atoms(binder)))
      case Some(c: ClassSymbol) =>
        library(c)
          .flatMap(BuiltinTypes.known(_, arguments))
          // What the standard model refuses is refused for good, not left to a later slice.
          .map(_.left.map(Unread.Refused(_)))
          .getOrElse(Left(Uncovered(s"a class: ${c.displayFullName}")))
      case _ => Left(Uncovered(s"a type: ${ref.showBasic}"))
    }

  // The fully qualified name of a top-level class under `scala` or `java` that the module does not
  // compile itself: the only names the standard model and the base types are asked.
  private def library(c: ClassSymbol): Option[String] =
    Readings
      .topLevel(c)
      .filter(name => (name.startsWith("scala.") || name.startsWith("java.")) && !own(c))

  // ---- definition rows ----------------------------------------------------------------------

  /** The size of a definition row. */
  def definition(tree: Tree): Either[String, Size] = tree match {
    case c: ClassDef if c.symbol.isModuleClass    => Right(UnitSize)
    case c: ClassDef if c.symbol.isEnum           => enumeration(c.symbol)
    case c: ClassDef if !c.symbol.isAbstractClass => stored(c.symbol)
    case _: ClassDef                              => Left("an abstract class or trait")
    case _: TypeMember                            => Left("a type alias")
    case _                                        => Left("a top-level value")
  }

  // What a class's constructor stores: the product of its fields.
  private def stored(c: ClassSymbol): Either[String, Size] =
    c.tree.toRight(s"a class without a tree: ${c.name}").flatMap { tree =>
      val constructor = tree.rhs.constr
      val fields = constructor.paramLists.flatMap(_.left.toOption.toList.flatten)
      if (c.typeParams.nonEmpty) Left("a class with type parameters")
      else if (!c.parents.forall(memberless) || c.givenSelfType.nonEmpty)
        Left("a class with parents")
      else if (constructor.symbol.visibility != Visibility.Public) Left("a restricted constructor")
      else Traversals.sequence(fields.map(f => base(f.tpt.toType))).map(_.foldLeft(UnitSize)(_ * _))
    }

  // An enum without parameters whose cases are all values: one value each.
  private def enumeration(c: ClassSymbol): Either[String, Size] =
    if (c.typeParams.nonEmpty || c.tree.exists(_.rhs.constr.paramLists.exists(_.merge.nonEmpty)))
      Left("an enum with parameters")
    else
      Traversals
        .sequence(c.sealedChildren.map {
          case _: TermSymbol  => Right(UnitSize)
          case _: ClassSymbol => Left("an enum with class cases")
        })
        .map(_.foldLeft(NothingSize)(_ + _))

  // A base type of the size algebra, by the class the compiler bound.
  private def base(tpe: Type): Either[String, Size] =
    classNamed(tpe.dealias)
      .flatMap(library)
      .flatMap(Readings.Bases.get)
      .flatMap(BaseTypes.get)
      .toRight(s"a field of type ${tpe.showBasic}")

}

private[compiled] object Readings {

  /** Why a type gives no shape: no slice reads it yet, or the counting model refuses it. */
  private enum Unread {
    case Uncovered(what: String)
    case Refused(reason: String)
  }

  // A top-level class's fully qualified name: its packages' and its own.
  private def topLevel(c: ClassSymbol): Option[String] = c.owner match {
    case p: PackageSymbol =>
      Some((p.fullName.path.map(_.toString) :+ c.name.toString).mkString("."))
    case _ => None
  }

  // Whether `symbol` is the member `name` of the top-level object `module` of `pkg`.
  private def member(symbol: Symbol, pkg: String, module: String, name: String): Boolean =
    symbol.name.toString == name && (symbol.owner match {
      case c: ClassSymbol => topLevel(c).contains(s"$pkg.$module")
      case _              => false
    })

  // The parents that declare nothing a count reads, the compiler's own for a case class among them.
  private val Memberless =
    Set("java.lang.Object", "scala.Product", "scala.Equals", "java.io.Serializable")

  // The base types of `BaseTypes` that are classes, by fully qualified name.
  private val Bases: Map[String, String] =
    (List(
      "Unit",
      "Boolean",
      "Byte",
      "Short",
      "Char",
      "Int",
      "Long",
      "Float",
      "Double",
      "Any",
      "Matchable",
      "Null"
    ).map(name => s"scala.$name" -> name) ++ List(
      "java.lang.String" -> "String",
      "java.lang.Object" -> "Object",
      "scala.math.BigInt" -> "BigInt",
      "scala.math.BigDecimal" -> "BigDecimal"
    )).toMap

}
