package cardinality.types

import scala.annotation.tailrec
import scala.meta.*

/** What the readers of a source ask of its syntax trees. */
private[cardinality] object Trees {

  /** A template a type or term names: a class, trait, object or enum. */
  type TemplateDefn = Defn.Class | Defn.Trait | Defn.Object | Defn.Enum

  /** A template a type names: a class, trait or enum. */
  type ClassDefn = Defn.Class | Defn.Trait | Defn.Enum

  /** A definition with parameter clauses and a body: a method, a given or an extension group. */
  type MethodDefn = Defn.Def | Defn.GivenAlias | Defn.Given | Defn.ExtensionGroup

  /** A set of trees by identity: two equal-looking trees at different places are different. */
  def identitySet(trees: Iterable[Tree]): java.util.Set[Tree] = {
    val set =
      java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
    trees.foreach(set.add)
    set
  }

  extension (mods: List[Mod]) {

    /** Whether the modifiers declare the definition `final`. */
    def hasFinal: Boolean = mods.exists(_.is[Mod.Final])

    /** Whether the modifiers declare the definition `sealed`. */
    def hasSealed: Boolean = mods.exists(_.is[Mod.Sealed])

    /** Whether the modifiers declare the type `opaque`. */
    def hasOpaque: Boolean = mods.exists(_.is[Mod.Opaque])
  }

  /** The source a tree is part of. */
  @tailrec
  def root(tree: Tree): Tree = tree.parent match {
    case Some(parent) => root(parent)
    case None         => tree
  }

  /** The trees around `tree`, innermost first. */
  def ancestors(tree: Tree): List[Tree] = List.unfold(tree.parent)(_.map(t => t -> t.parent))

  /** The first answer `f` gives on the trees around `from`, innermost first. */
  @tailrec
  def innermost[A](from: Tree)(f: Tree => Option[A]): Option[A] = from.parent match {
    case None        => None
    case Some(scope) =>
      f(scope) match {
        case None  => innermost(scope)(f)
        case found => found
      }
  }

  /** Whether one package clause or package object declares a member of its package: a package
    * object does, a clause by a statement other than a nested clause, a nested package object (a
    * member of the nested package) or an import. Both sides read a package a supplied file declares
    * a member in as complete (the closed-package assumption, docs/research/stdlib-model.md).
    */
  def declaresPackageMember(stats: List[Stat], packageObject: Boolean): Boolean =
    packageObject || stats.exists(stat => !stat.is[Pkg] && !stat.is[Pkg.Object] && !stat.is[Import])

  /** The statement of a package clause's body, or of a file, that a tree is written in. */
  def topLevelStatement(tree: Tree): Option[Tree] =
    (tree :: ancestors(tree)).find(_.parent.exists(p => p.is[Pkg.Body] || p.is[Source]))

  /** The constructor a type applies (`C` of `C[A]`), or the type itself. */
  def callee(tpe: Type): Type = tpe match {
    case Type.Apply.After_4_6_0(c, _) => c
    case other                        => other
  }

  /** Whether a type is spelled with this name as its last segment, applied or not. */
  @tailrec
  def spells(tpe: Type, name: String): Boolean = tpe match {
    case Type.Apply.After_4_6_0(callee, _) => spells(callee, name)
    case Type.Name(n)                      => n == name
    case Type.Select(_, Type.Name(n))      => n == name
    case _                                 => false
  }

  /** The types a constructor's parameters declare. */
  def fields(ctor: Ctor.Primary): List[Type] =
    ctor.paramClauses.flatMap(_.values).flatMap(_.decltpe).toList

  /** A class, trait, enum or alias of that name: a definition of the type. */
  def typeNamed(
      name: String
  ): PartialFunction[Tree, ClassDefn | Defn.Type] = {
    case d: (ClassDefn | Defn.Type) if d.name.value == name => d
  }

  /** The types a value of a tuple, union, intersection, by-name or repeated type holds as they are:
    * a value of each is a value of the whole, or its part.
    */
  object Held {

    def unapply(tpe: Type): Option[List[Type]] = tpe match {
      case Type.Tuple(args)                                   => Some(args)
      case Type.ApplyInfix(l, Type.Name("|" | "&" | "*:"), r) => Some(List(l, r))
      case Type.ByName(inner)                                 => Some(List(inner))
      case Type.Repeated(inner)                               => Some(List(inner))
      case _                                                  => None
    }

  }

}
