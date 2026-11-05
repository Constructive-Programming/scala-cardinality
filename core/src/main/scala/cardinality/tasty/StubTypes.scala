package cardinality.tasty

import scala.meta.inputs.Input
import scala.meta.parsers.Parse
import scala.meta.{dialects, Parsed, Source, Stat, XtensionSyntax}

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.SourceLanguage
import tastyquery.Symbols.*
import tastyquery.Types.*

/** Spelling shared by the stub renderers: identifiers, owner paths and the two reserved names. */
private[tasty] object Spell {

  /** The body of every concrete compiled member: a name no source declares, so a reading that looks
    * into the body refuses, as for any unresolved callee, instead of reading the body as absent
    * (abstract) or as a throw (`???`).
    */
  val Body = "_root_.compiledBodyNotRead"

  /** The type of a member whose compiled signature has no source spelling here. */
  val Unread = "_root_.SignatureNotRead"

  final class Unsupported(what: String) extends RuntimeException(what)

  def source(text: String): Parsed[Source] =
    summon[Parse[Source]].apply(Input.String(text), dialects.Scala3)

  def parses(text: String): Boolean =
    summon[Parse[Stat]].apply(Input.String(text), dialects.Scala3).isInstanceOf[Parsed.Success[?]]

  // Scala 3's own hard keywords, which scalameta's default spelling leaves unquoted.
  private val scala3Keywords = Set("enum", "export", "given", "then")

  private def quoted(name: String, spelled: String): String =
    if (scala3Keywords(name)) s"`$name`" else spelled

  def term(name: String): String = quoted(name, scala.meta.Term.Name(name).syntax)

  def tpe(name: String): String = quoted(name, scala.meta.Type.Name(name).syntax)

  /** A term name followed by a `:` needs a space when it ends in an operator character. */
  def beforeColon(name: String): String = {
    val spelled = term(name)
    if (spelled.last.isLetterOrDigit || spelled.last == '`' || spelled.last == '_') spelled
    else spelled + " "
  }

  def packageObject(c: ClassSymbol): Boolean = c.name match {
    case n: ObjectClassTypeName =>
      val s = n.underlying.toString
      s.endsWith("$package") || s == "package"
    case _ => false
  }

  def objectName(c: ClassSymbol): String = c.name match {
    case n: ObjectClassTypeName => n.underlying.toString
    case n                      => n.toString
  }

  /** A static owner's term path: packages and objects, a package object standing for its package.
    */
  def path(s: Symbol)(using Context): String = s match {
    case p: PackageSymbol =>
      if (p.isRootPackage) "_root_" else p.fullName.path.map(n => term(n.toString)).mkString(".")
    case c: ClassSymbol if c.isModuleClass =>
      if (packageObject(c)) path(c.owner) else path(c.owner) + "." + term(objectName(c))
    case other => throw Unsupported(s"path through $other")
  }

  def fullName(t: Type)(using Context): String = t match {
    case r: TypeRef     => r.optSymbol.map(_.displayFullName).getOrElse("")
    case a: AppliedType => fullName(a.tycon)
    case _              => ""
  }

}

/** Spells compiled types as Scala 3 source, fully qualified. `enclosing` lists the classes being
  * rendered, innermost first: a member of the innermost one is spelled by its simple name.
  */
final private[tasty] class StubTypes(enclosing: List[ClassSymbol])(using Context) {
  import Spell.*

  private def prefix(p: Prefix): String = p match {
    case NoPrefix      => ""
    case p: PackageRef => path(p.symbol) + "."
    case t: ThisType   =>
      val c = thisClass(t)
      if (c.isModuleClass) path(c) + "."
      else if (!enclosing.headOption.contains(c) && enclosing.contains(c))
        tpe(c.name.toString) + ".this."
      else ""
    case t: TermRef =>
      t.optSymbol match {
        case Some(s) if s.isModuleVal && s.moduleClass.exists(packageObject) => prefix(t.prefix)
        case _ => prefix(t.prefix) + term(t.name.toString) + "."
      }
    case t: TermParamRef => term(t.paramName.toString) + "."
    case t: Type         => apply(t) + "#"
  }

  // Java's `Object` in a signature: Scala reads it as `Any` (each conforms to the other).
  private def javaObject(t: Type): Boolean = t match {
    case r: TypeRef => r.isFromJavaObject
    case _          => false
  }

  // A Java signature has no higher kinds: a generic class named without arguments is raw.
  private val javaSignature = enclosing.headOption.exists(_.sourceLanguage == SourceLanguage.Java)

  def apply(t: TypeMappable): String = t match {
    case t: TypeRef if javaObject(t) => "scala.Any"
    case t: TypeRef                  =>
      t.optSymbol match {
        case Some(c: ClassSymbol) if javaSignature && c.typeParams.nonEmpty =>
          throw Unsupported(s"raw type ${c.displayFullName}")
        case _ => constructor(t)
      }
    case t => spelled(t)
  }

  private def constructor(t: TypeRef): String = t.optSymbol match {
    case Some(c: ClassSymbol) if c.isModuleClass && !packageObject(c) => path(c) + ".type"
    case _ => prefix(t.prefix) + tpe(t.name.toString)
  }

  private def spelled(t: TypeMappable): String = t match {
    case _: NothingType              => "scala.Nothing"
    case _: AnyKindType              => "scala.AnyKind"
    case t: (TermRef | TermParamRef) => prefix(t).stripSuffix(".") + ".type"
    case t: ThisType                 =>
      val c = thisClass(t)
      if (c.isModuleClass) path(c) + ".type"
      else if (enclosing.headOption.contains(c)) "this.type"
      else tpe(c.name.toString) + ".this.type"
    case t: ConstantType  => literal(t.value.value)
    case t: AppliedType   => applied(t)
    case t: FlexibleType  => apply(t.nonNullableType)
    case t: ByNameType    => "=> " + apply(t.resultType)
    case t: RepeatedType  => apply(t.elemType) + "*"
    case t: AnnotatedType => apply(t.typ)
    case t: TypeParamRef  =>
      val name = t.paramName.toString
      if (name.startsWith("_$")) "_" else tpe(name)
    case t: TypeLambda     => lambdaParams(t) + " =>> " + apply(t.resultType)
    case t: OrType         => s"(${apply(t.first)} | ${apply(t.second)})"
    case t: AndType        => s"(${apply(t.first)} & ${apply(t.second)})"
    case t: TypeRefinement =>
      val name = tpe(t.refinedName.toString)
      refine(t.parent, s"type $name${bounds(t.refinedBounds)}")
    case t: TermRefinement =>
      val keyword = if (t.isStable) "val" else "def"
      refine(t.parent, s"$keyword ${term(t.refinedName.toString)}${methodic(t.refinedType)}")
    case t: WildcardTypeArg => "?" + bounds(t.bounds)
    case t: MatchType       =>
      val cases = t.cases.map(c => s"case ${apply(c.pattern)} => ${apply(c.result)}")
      s"${apply(t.scrutinee)} match {\n${cases.mkString("\n")}\n}"
    case other => throw Unsupported(other.getClass.getSimpleName)
  }

  private def thisClass(t: ThisType): ClassSymbol = t.tref.optSymbol match {
    case Some(c: ClassSymbol) => c
    case other                => throw Unsupported(s"this of $other")
  }

  private def binders(names: List[Name], bounds: List[TypeBounds]): String =
    names.zip(bounds).map((n, b) => tpe(n.toString) + this.bounds(b)).mkString("[", ", ", "]")

  def lambdaParams(t: TypeLambda): String = binders(t.paramNames, t.paramTypeBounds)

  private def refine(parent: Type, member: String): String = parent match {
    case p: RefinedType => apply(p).stripSuffix(" }") + "; " + member + " }"
    case p              => apply(p) + " { " + member + " }"
  }

  private def methodic(t: TypeOrMethodic): String = t match {
    case m: MethodType =>
      val keyword = if (m.isContextual) "using " else if (m.isImplicit) "implicit " else ""
      val params =
        m.paramNames.zip(m.paramTypes).map((n, p) => beforeColon(n.toString) + ": " + apply(p))
      params.mkString("(" + keyword, ", ", ")") + methodic(m.resultType)
    case p: PolyType => binders(p.paramNames, p.paramTypeBounds) + methodic(p.resultType)
    case t: Type     => ": " + apply(t)
  }

  /** Bounds as written after a binder: a higher-kinded binder gets its parameter list. */
  def bounds(b: TypeBounds): String = b match {
    case TypeAlias(a) => " = " + apply(a)
    case b            =>
      val kind = b.high match {
        case l: TypeLambda => l.paramNames.map(n => tpe(n.toString)).mkString("[", ", ", "]")
        case _             => ""
      }
      kind + lower(b.low) + upper(b.high)
  }

  private def named(t: Type, name: String): Boolean = t match {
    case r: TypeRef => r.optSymbol.exists(_.displayFullName == name)
    case _          => false
  }

  private def lower(t: Type): String = t match {
    case _: NothingType                                          => ""
    case t if named(t, "scala.Nothing")                          => ""
    case l: TypeLambda if named(l.resultType, "scala.Nothing")   => ""
    case l: TypeLambda if l.resultType.isInstanceOf[NothingType] => ""
    case l: TypeLambda                                           => " >: " + apply(l.resultType)
    case t                                                       => " >: " + apply(t)
  }

  private def upper(t: Type): String = t match {
    case l: TypeLambda => if (named(l.resultType, "scala.Any")) "" else " <: " + apply(l.resultType)
    case t if named(t, "scala.Any") || javaObject(t) => ""
    case t                                           => " <: " + apply(t)
  }

  private val FunctionN = "scala\\.Function\\d+".r
  private val ContextFunctionN = "scala\\.ContextFunction\\d+".r
  private val TupleN = "scala\\.Tuple\\d+".r

  private def applied(t: AppliedType): String = {
    val args = t.args.map(apply)
    fullName(t.tycon) match {
      case FunctionN() =>
        val params = args.init
        val single =
          params.size == 1 && !params.head.startsWith("(") && !params.head.startsWith("=>")
        (if (single) params.head else params.mkString("(", ", ", ")")) + " => " + args.last
      case ContextFunctionN() => args.init.mkString("(", ", ", ")") + " ?=> " + args.last
      case TupleN()           => args.mkString("(", ", ", ")")
      case "scala.|" | "scala.&" if args.size == 2 =>
        s"(${args.head} ${fullName(t.tycon).drop(6)} ${args(1)})"
      case _ =>
        (t.tycon match {
          case r: TypeRef => constructor(r)
          case other      => apply(other)
        }) + args.mkString("[", ", ", "]")
    }
  }

  private def literal(v: Any): String = v match {
    case s: String => scala.meta.Lit.String(s).syntax
    case c: Char   => scala.meta.Lit.Char(c).syntax
    case l: Long   => s"${l}L"
    case f: Float  => s"${f}f"
    case d: Double => s"${d}d"
    case ()        => "scala.Unit"
    case null      => "scala.Null"
    case other     => other.toString
  }

}
