package cardinality.tasty

import scala.util.control.NonFatal

import tastyquery.Contexts.*
import tastyquery.Modifiers.*
import tastyquery.Symbols.*
import tastyquery.Trees
import tastyquery.Types.*

/** Renders a compiled class's declarations as a Scala 3 signature stub: every name, modifier,
  * parameter clause, parent and type the class declares, with each concrete body replaced by
  * `Spell.Body`. A declaration whose signature has no spelling here keeps its name with the
  * `Spell.Unread` type, so a reading through it refuses instead of missing it.
  */
final private[tasty] class StubDeclarations(using Context) {
  import Spell.*

  // Parents every class has, and the ones the compiler adds to case classes, objects and enums.
  private val universal = Set("java.lang.Object", "scala.Any", "scala.AnyRef", "scala.Matchable")

  private val synthesized = Set(
    "scala.Product",
    "scala.Equals",
    "java.io.Serializable",
    "scala.Serializable",
    "scala.reflect.Enum",
    "scala.deriving.Mirror.Product",
    "scala.deriving.Mirror.Singleton",
    "scala.deriving.Mirror.Sum"
  )

  private def scope(s: DeclaringSymbol): String = tpe(s.name.toString.stripSuffix("$"))

  private def visibility(v: Visibility): String = v match {
    case Visibility.PrivateThis | Visibility.Private     => "private "
    case Visibility.ScopedPrivate(s)                     => s"private[${scope(s)}] "
    case Visibility.ProtectedThis | Visibility.Protected => "protected "
    case Visibility.ScopedProtected(s)                   => s"protected[${scope(s)}] "
    case Visibility.Public                               => ""
  }

  private def overrides(s: TermOrTypeSymbol): Boolean =
    try s.nextOverriddenSymbol.isDefined
    catch { case NonFatal(_) => false }

  private def termModifiers(t: TermSymbol): String = {
    val flags = List(
      (t.isAbstractOverride, "abstract override"),
      (!t.isAbstractOverride && overrides(t), "override"),
      (t.isFinalMember, "final"),
      (t.isImplicit, "implicit"),
      (t.isTransparentInline, "transparent"),
      (t.isInline, "inline"),
      (t.isInfix, "infix"),
      (t.kind == TermSymbolKind.LazyVal, "lazy")
    )
    visibility(t.visibility) + flags.collect { case (true, m) => m + " " }.mkString
  }

  private def classModifiers(c: ClassSymbol): String =
    if (c.isEnum) visibility(c.visibility) else levelModifiers(c)

  private def levelModifiers(c: ClassSymbol): String = {
    val level = c.openLevel match {
      case OpenLevel.Final if !c.isModuleClass => "final "
      case OpenLevel.Sealed                    => "sealed "
      case OpenLevel.Open                      => "open "
      case _                                   => ""
    }
    val flags = List(
      (c.isAbstractClass && !c.isTrait, "abstract"),
      (c.isCaseClass && !c.isEnum, "case")
    )
    visibility(c.visibility) + level + flags.collect { case (true, m) => m + " " }.mkString
  }

  private def skipped(s: Symbol): Boolean = s match {
    case t: TermSymbol =>
      t.isSynthetic || t.isSetter || t.isModuleVal || t.name.toString.contains("$") ||
      t.name.toString == "<init>" || (t.isParamAccessor && !t.isMethod)
    case c: ClassSymbol      => c.isSynthetic || objectName(c).contains("$")
    case t: TypeMemberSymbol => t.isSynthetic || t.name.toString.contains("$")
    case _                   => true
  }

  private def typeParam(t: TypeParamSymbol, types: StubTypes): String = {
    val variance = t match {
      case c: ClassTypeParamSymbol =>
        c.declaredVariance match {
          case Variance.Covariant     => "+"
          case Variance.Contravariant => "-"
          case _                      => ""
        }
      case _ => ""
    }
    variance + tpe(t.name.toString) + types.bounds(t.declaredBounds)
  }

  /** One term parameter clause; a constructor's clause spells its fields' `val`/`var`. */
  private def clause(
      ps: List[TermSymbol],
      types: StubTypes,
      fields: Option[ClassSymbol]
  ): String = {
    val keyword =
      if (ps.headOption.exists(_.isGivenOrUsing)) "using "
      else if (ps.headOption.exists(_.isImplicit)) "implicit "
      else ""
    ps.map { p =>
      val field = fields.flatMap(_.getDecl(p.name).collect {
        case f: TermSymbol if f.isParamAccessor && f.visibility != Visibility.PrivateThis => f
      })
      val holder = field.fold("")(f =>
        visibility(f.visibility) + (if (f.kind == TermSymbolKind.Var) "var " else "val ")
      )
      val inline = if (p.isInline) "inline " else ""
      val default = if (p.isParamWithDefault) " = " + Body else ""
      holder + inline + beforeColon(p.name.toString) + ": " + types(p.declaredType) + default
    }.mkString("(" + keyword, ", ", ")")
  }

  private def clauses(cs: List[ParamSymbolsClause], types: StubTypes): String = {
    def merged(cs: List[ParamSymbolsClause]): List[ParamSymbolsClause] = cs match {
      case Right(a) :: Right(b) :: rest => merged(Right(a ++ b) :: rest)
      case c :: rest                    => c :: merged(rest)
      case Nil                          => Nil
    }
    merged(cs).map {
      case Right(ts) => ts.map(typeParam(_, types)).mkString("[", ", ", "]")
      case Left(ps)  => clause(ps, types, None)
    }.mkString
  }

  private def result(t: TypeOrMethodic): Type = t match {
    case m: MethodType => result(m.resultType)
    case p: PolyType   => result(p.resultType)
    case t: Type       => t
  }

  private def term(s: TermSymbol, enclosing: List[ClassSymbol]): String = {
    val types = StubTypes(enclosing)
    val body = if (s.isAbstractMember) "" else " = " + Body
    val mods = termModifiers(s)
    val name = s.name.toString
    s.kind match {
      case TermSymbolKind.Def =>
        // An extension's leading type clause and receiver are its `extension` clause.
        val all = s.paramSymss
        val lead = if (s.isExtensionMethod) all.takeWhile(_.isRight) else Nil
        val receiver = if (s.isExtensionMethod) all.drop(lead.size).take(1) else Nil
        val rest = all.drop(lead.size + receiver.size)
        val head =
          if (s.isExtensionMethod) "extension " + clauses(lead ++ receiver, types) + " " else ""
        val spelled = if (rest.isEmpty) beforeColon(name) else Spell.term(name)
        val keyword = if (s.isGivenOrUsing && !s.isExtensionMethod) "given" else "def"
        s"$head${mods}$keyword $spelled${clauses(rest, types)}: ${types(result(s.declaredType))}$body"
      case TermSymbolKind.Val | TermSymbolKind.LazyVal =>
        val keyword = if (s.isGivenOrUsing) "given" else "val"
        s"${mods}$keyword ${beforeColon(name)}: ${types(s.declaredType)}$body"
      case TermSymbolKind.Var => s"${mods}var ${beforeColon(name)}: ${types(s.declaredType)}$body"
      case TermSymbolKind.Module => ""
    }
  }

  private def typeMember(s: TypeMemberSymbol, enclosing: List[ClassSymbol]): String = {
    val types = StubTypes(enclosing)
    val head = visibility(s.visibility) + (if (s.isOpaqueTypeAlias) "opaque " else "") +
      "type " + tpe(s.name.toString)
    // ponytail: a type lambda's parameter variances are not in tasty-query's model, so an alias's
    // parameters read invariant; read the TypeMember tree's binders if a count needs them.
    def alias(a: Type): String = a match {
      case l: TypeLambda => types.lambdaParams(l) + " = " + types(l.resultType)
      case a             => " = " + types(a)
    }
    s.typeDef match {
      case d: TypeMemberDefinition.TypeAlias       => head + alias(d.alias)
      case d: TypeMemberDefinition.AbstractType    => head + types.bounds(d.bounds)
      case d: TypeMemberDefinition.OpaqueTypeAlias =>
        d.alias match {
          case _: TypeLambda => head + alias(d.alias)
          case _             => head + types.bounds(d.bounds) + alias(d.alias)
        }
    }
  }

  private def guarded(text: => String, fallback: String): String = {
    val spelled =
      try text
      catch { case NonFatal(_) => fallback }
    if (spelled.isEmpty || parses(spelled)) spelled else fallback
  }

  // Declarations in source order: tasty-query keeps them hashed, overloads in no fixed order.
  private def ordered(c: ClassSymbol): List[TermOrTypeSymbol] = {
    def position(d: Symbol): (Int, Int) =
      try
        d.tree
          .filterNot(_.pos.isUnknown)
          .fold((Int.MaxValue, 0))(t => (t.pos.startLine, t.pos.startColumn))
      catch { case NonFatal(_) => (Int.MaxValue, 0) }
    def signature(d: Symbol): String = d match {
      case t: TermSymbol => attempt(t.signature.toString, "")
      case _             => ""
    }
    c.declarations.sortBy(d => (position(d), d.name.toString, signature(d)))
  }

  private def attempt(text: => String, fallback: String): String =
    try text
    catch { case NonFatal(_) => fallback }

  /** The primary constructor: the template's own, not one of the secondary `this` overloads. */
  private def primaryConstructor(c: ClassSymbol): Option[TermSymbol] =
    attemptOption(c.tree.map(_.rhs.constr.symbol))
      .orElse(ordered(c).collectFirst { case t: TermSymbol if t.name.toString == "<init>" => t })

  private def attemptOption[A](value: => Option[A]): Option[A] =
    try value
    catch { case NonFatal(_) => None }

  private def members(c: ClassSymbol, enclosing: List[ClassSymbol]): List[String] =
    ordered(c)
      .filterNot(skipped)
      .flatMap {
        case t: TermSymbol if c.isEnum || !t.isEnumCase =>
          List(
            guarded(
              term(t, enclosing),
              s"${termModifiers(t)}def ${beforeColon(t.name.toString)}: $Unread"
            )
          )
        case t: TypeMemberSymbol =>
          List(guarded(typeMember(t, enclosing), s"type ${tpe(t.name.toString)} = $Unread"))
        case k: ClassSymbol if !(k.isEnum && k.isCaseClass) => List(classText(k, enclosing))
        case _                                              => Nil
      }
      .filter(_.nonEmpty)

  /** A parent's constructor argument lists, from the class's template, each argument unread. */
  private def packageOf(s: Symbol): Option[PackageSymbol] = s match {
    case p: PackageSymbol => Some(p)
    case other            => Option(other.owner).flatMap(packageOf)
  }

  /** A parent spelled as its source would: its simple name when it is a type of the class's own
    * package that no enclosing template's type of the name hides (the analyzer reads a sealed
    * family's subtypes by the simple name they extend), else fully qualified.
    */
  private def parentName(
      p: Type,
      c: ClassSymbol,
      outer: List[ClassSymbol],
      types: StubTypes
  ): String = {
    val spelled = types(p)
    val head = p match {
      case a: AppliedType => a.tycon
      case t              => t
    }
    head match {
      case r: TypeRef =>
        r.optSymbol
          .collect {
            case pc: ClassSymbol
                if pc.owner.isPackage && packageOf(pc) == packageOf(c) &&
                  !outer.exists(_.declarations.exists(d => d.isType && d.name == pc.name)) =>
              spelled.stripPrefix(path(pc.owner) + ".")
          }
          .getOrElse(spelled)
      case _ => spelled
    }
  }

  private def parents(c: ClassSymbol, outer: List[ClassSymbol], types: StubTypes): List[String] = {
    val dropped =
      universal ++ (if (c.isCaseClass || c.isEnum || c.isModuleClass) synthesized else Set())
    def lists(t: Trees.TermTree): List[Int] = t match {
      case Trees.Apply(f, args) => lists(f) :+ args.size
      case _                    => Nil
    }
    val shapes = c.tree.toList.flatMap(_.rhs.parents).map {
      case a: Trees.Apply => lists(a)
      case _              => Nil
    }
    val declared = c.parents
    val aligned = if (shapes.size == declared.size) shapes else declared.map(_ => Nil)
    declared.zip(aligned).flatMap { (p, shape) =>
      if (dropped(types(p).takeWhile(_ != '['))) Nil
      else {
        // A context clause of the parent's constructor is filled by the compiler: not spelled.
        val explicit = (p match {
          case a: AppliedType => a.tycon
          case t              => t
        }) match {
          case r: TypeRef =>
            r.optSymbol
              .collect { case pc: ClassSymbol => pc }
              .toList
              .flatMap(pc =>
                primaryConstructor(pc).toList
                  .flatMap(_.paramSymss.collect {
                    case Left(ps) =>
                      !ps.headOption.exists(x => x.isGivenOrUsing || x.isImplicit)
                  })
              )
          case _ => Nil
        }
        val args = shape.zipWithIndex.collect {
          case (n, i) if explicit.lift(i).getOrElse(true) =>
            List.fill(n)(Body).mkString("(", ", ", ")")
        }
        List(parentName(p, c, outer, types) + args.mkString)
      }
    }
  }

  private def enumCases(c: ClassSymbol, enclosing: List[ClassSymbol]): List[String] =
    c.companionClass.toList.flatMap(ordered).flatMap {
      case t: TermSymbol if t.isEnumCase && !t.isSynthetic =>
        List("case " + Spell.term(t.name.toString))
      case k: ClassSymbol if k.isEnum && k.isCaseClass =>
        val types = StubTypes(k :: enclosing)
        val ext = parents(k, enclosing, types)
        List(
          guarded(
            s"case ${tpe(k.name.toString)}${typeParams(k, types)}${constructor(k, types)}" +
              (if (ext.isEmpty) "" else " extends " + ext.mkString(", ")),
            "case " + tpe(k.name.toString)
          )
        )
      case _ => Nil
    }

  private def typeParams(c: ClassSymbol, types: StubTypes): String =
    if (c.typeParams.isEmpty) "" else c.typeParams.map(typeParam(_, types)).mkString("[", ", ", "]")

  // ponytail: secondary constructors are not rendered; a class they build reads only its primary.
  // A trait's `<init>` carries its trait parameters.
  private def constructor(c: ClassSymbol, types: StubTypes): String =
    primaryConstructor(c)
      .fold("") { k =>
        val access = k.visibility match {
          case Visibility.Public => ""
          case v                 => " " + visibility(v).trim
        }
        val clauses = k.paramSymss.collect { case Left(ps) => ps }
        // A trait is written with its parameters only when it has some (`trait T(val x: Int)`).
        if (c.isTrait && clauses.forall(_.isEmpty)) ""
        else access + clauses.map(clause(_, types, Some(c))).mkString
      }

  /** A class, object, trait or enum with its body. A fragment that has no spelling here (a parent,
    * the constructor, the type parameters, the self type) is not dropped: the class then extends
    * the undeclared `Spell.Unread`, so a reading of its members or parents refuses instead of
    * reading fewer of them. The same holds when the whole text does not parse.
    */
  def classText(c: ClassSymbol, outer: List[ClassSymbol]): String = {
    val enclosing = c :: outer
    val types = StubTypes(enclosing)
    def spelled[A](fragment: => A): Option[A] =
      try Some(fragment)
      catch { case NonFatal(_) => None }
    val ext = spelled(parents(c, outer, types))
    val self = spelled(
      c.givenSelfType
        .filterNot(_ => c.isModuleClass)
        .map(types(_))
        // A self type that is the class itself is not written: the compiler records one anyway.
        .filterNot(_ == types(c.appliedRefInsideThis))
        .fold("")(t => s" self: $t =>")
    )
    val ctor = spelled(constructor(c, types))
    val tparams = spelled(typeParams(c, types))
    val complete = ext.nonEmpty && self.nonEmpty && ctor.nonEmpty && tparams.nonEmpty
    def extending(ps: List[String]) = if (ps.isEmpty) "" else " extends " + ps.mkString(", ")
    val unread = extending(List(Unread))
    val head =
      if (c.isModuleClass) s"${classModifiers(c)}object ${Spell.term(objectName(c))}"
      else {
        val keyword = if (c.isEnum) "enum" else if (c.isTrait) "trait" else "class"
        s"${classModifiers(c)}$keyword ${tpe(c.name.toString)}${tparams.getOrElse("")}" +
          ctor.getOrElse("")
      }
    val body = (if (c.isEnum) enumCases(c, outer) else Nil) ++ members(c, enclosing)
    val block = s" {${self.getOrElse("")}\n${body.map("  " + _).mkString("\n")}\n}"
    val text = head + (if (complete) extending(ext.getOrElse(Nil)) else unread) + block
    if (parses(text)) text else head + unread
  }

  /** A top-level class or object; a package object's members stand at the package's top level. */
  def topLevel(c: ClassSymbol): List[String] =
    if (packageObject(c)) members(c, List(c)) else List(classText(c, Nil))

}
