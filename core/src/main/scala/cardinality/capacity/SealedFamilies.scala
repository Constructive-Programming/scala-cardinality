package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.capacity.ParentNames.Denotes
import cardinality.types.*

// Sealed families as tagged sums (docs/research/stored-value-families.md). `sealed` is the
// compiler's promise that every direct subtype of a trait or abstract class is declared in its own
// source file, and pattern matching on the runtime class tells the subtypes apart, so the parent's
// value space is the sum of its subtypes' — Σ over direct subtypes D of |D|, recursion included —
// exactly when every value of the family is a value of one named subtype the file declares, and
// each subtype is a member of every instantiation of the parent. Where either fails the family is
// not summed, and every reading of it says why.
private[cardinality] object SealedFamilies {

  // Trees are parsed and reprinted under Scala 3 dialect rules.
  private given scala3: Dialect = dialects.Scala3

  /** A sealed parent a system declares, under its key: the subtypes it sums, or why it sums none.
    */
  final case class Family(
      key: TypeName,
      parent: Defn.Trait | Defn.Class,
      summands: Either[String, List[Child]]
  )

  // One statement list of a system, with the keys it solves its definitions under and the objects
  // it defines, whose bodies its families' subtypes may sit in.
  final private case class Level(
      stats: List[Stat],
      key: TypeName => TypeName,
      modules: List[Namespaces.Module]
  )

  // A subtype as its declaration found it: the summand it is, or the sealed intermediate whose own
  // sum it contributes.
  final private case class Found(child: Child, intermediate: Boolean)

  /** The names a source set lets stand for another type: each alias, for the names its body
    * mentions, and each name an import or export renames a type to (`P => Q`, `P as Q`), for the
    * renamed one — read once per source set.
    */
  final case class Synonyms(
      entries: List[(String, Set[String])],
      renamed: Set[String],
      read: List[Tree] = Nil
  ) {

    def ++(other: Synonyms): Synonyms =
      Synonyms(entries ++ other.entries, renamed ++ other.renamed, read ++ other.read)

    /** Whether the synonyms already hold this file's (by identity: the file's own tree). */
    def holds(file: Tree): Boolean = readFiles.contains(file)

    private lazy val readFiles: java.util.Set[Tree] = cardinality.types.Trees.identitySet(read)

    // Each word to the synonyms that spell it, so a closure follows only what a name reaches.
    private lazy val spelledBy: Map[String, List[String]] =
      entries.flatMap((n, words) => words.map(_ -> n)).groupMap(_._1)(_._2)

    // The names a parent may be extended by: its own, and every synonym that may stand for it or
    // for another such synonym — a superset, so a family is refused rather than summed short of a
    // subtype written against one (`type WA = W; … extends WA`, `import x.{W => V}; … extends V`).
    def of(parent: String): Set[String] = {
      @tailrec def close(frontier: List[String], found: Set[String]): Set[String] =
        frontier match {
          case Nil          => found
          case word :: rest =>
            val fresh = spelledBy.getOrElse(word, Nil).filterNot(found)
            close(fresh ++ rest, found ++ fresh)
        }
      close(List(parent), Set(parent))
    }

  }

  object Synonyms {

    val none: Synonyms = Synonyms(Nil, Set.empty)

    def of(trees: List[Tree]): Synonyms = {
      val found = trees.flatMap(_.collect {
        case d: Defn.Type              => (d.name.value -> TypeName.words(d.body).toSet, false)
        case Importee.Rename(from, to) => (to.value -> Set(from.value), true)
      })
      Synonyms(found.map(_._1), found.collect { case ((n, _), true) => n }.toSet, trees)
    }

  }

  /** The sealed families a system's bodies and their objects (at any depth) declare; `synonyms` are
    * the other supplied sources' (a subtype may extend any of them), and `site` the view each
    * statement of the system reads its own syntax in.
    */
  def of(
      bodies: List[List[Stat]],
      modules: List[Namespaces.Module],
      synonyms: Synonyms = Synonyms.none,
      site: Stat => Option[Scope => Scope] = _ => Some(identity)
  ): List[Family] = {
    val levels =
      bodies.map(stats => Level(stats, identity, Namespaces.modules(stats))) ++
        Namespaces.everyModule(modules).map(m => Level(m.stats, m.key, m.nested))
    val files = levels.flatMap(_.stats.headOption.map(Trees.root)).foldLeft(List.empty[Tree]) {
      (acc, file) => if (acc.exists(_ eq file)) acc else file :: acc
    }
    // A file the given synonyms were read from adds nothing they lack.
    // Kept as the same instance when nothing is added, so its indexes are read once.
    val unread = files.filterNot(synonyms.holds)
    val names = if (unread.isEmpty) synonyms else synonyms ++ Synonyms.of(unread)
    val first = levels.flatMap(level => sealedParents(level).map(read(level, _, names, site)))
    val found = first.map { (family, subtypes) =>
      family.key -> Option.when(family.summands.isRight)(subtypes)
    }.toMap
    first.map((family, subtypes) => resolve(family, subtypes, found))
  }

  /** The keys each summed family passes through to its subtypes: the solver's dependency edges. */
  def edges(families: List[Family]): Map[TypeName, List[TypeName]] =
    families
      .flatMap(f => f.summands.toOption.map(cs => f.key -> cs.filterNot(_.module).map(_.name)))
      .toMap

  /** Each summed family's equation, under its key. */
  def equations(families: List[Family]): List[(TypeName, Named)] =
    families.flatMap(f => f.summands.toOption.map(children => f.key -> equation(f, children)))

  /** Why each family that is not summed is not, under its key. */
  def unsummed(families: List[Family]): List[(TypeName, String)] =
    families.flatMap(f => f.summands.left.toOption.map(f.key -> _))

  // The sealed traits and sealed abstract classes a statement list declares.
  private def sealedParents(level: Level): List[Defn.Trait | Defn.Class] =
    level.stats.collect {
      case d: Defn.Trait if Counter.isSealed(d)                               => d
      case d: Defn.Class if Counter.isSealed(d) && Counter.isAbstractClass(d) => d
    }

  // A first reading: the subtypes the parent's file declares, each where the sum can read it, or
  // why the family is not their sum (with no subtypes). A subtype counts when its clause spells the
  // parent plainly and that spelling denotes this parent where it is written (`ParentNames`): an
  // object that inherits or exports a type of the parent's name rebinds it for what it holds.
  private def read(
      level: Level,
      parent: Defn.Trait | Defn.Class,
      synonyms: Synonyms,
      site: Stat => Option[Scope => Scope]
  ): (Family, List[Found]) = {
    val name = Walk.named(parent).value
    val key = level.key(TypeName.of(name))
    def denotes(tree: Tree) = ParentNames.denotes(tree, parent, name)
    val declared = candidates(level, name).filter((d, _) => denotes(d) == Denotes.Parent)
    val spellings = synonyms.of(name)
    val stray = written(parent, spellings).collectFirst(Function.unlift { w =>
      val plainly = inits(w).filter(init => spellings.exists(named(init.tpe, _)))
      val status =
        if (plainly.forall(i => SealedSlots.plainCallee(i.tpe).contains(name))) denotes(w)
        else Denotes.Unknown
      Option.when(status != Denotes.Other && !declared.exists(_._1 eq w))(
        describe(w, name, status, synonyms.renamed)
      )
    })
    val slots = SealedSlots.Parent(name, parent.tparamClause.values)
    val subtypes = stray
      .toLeft(declared)
      .flatMap(ds => Traversals.sequence(ds.map((d, at) => summand(d, at, slots, site))))
    (Family(key, parent, subtypes.map(_.map(_.child))), subtypes.getOrElse(Nil))
  }

  // A family whose subtypes include a sealed intermediate is summed only when the intermediate is,
  // and only when no subtype is reached twice: an overlapping summand would count its values twice.
  private def resolve(
      family: Family,
      subtypes: List[Found],
      found: Map[TypeName, Option[List[Found]]]
  ): Family = {
    val intermediates = subtypes.filter(_.intermediate).map(_.child.name)
    val reached = intermediates.map(i => i -> leaves(i, found, Set(family.key)))
    val why = reached
      .collectFirst { case (i, None) => s"its sealed subtype ${i.value} is not summed" }
      .orElse {
        val all =
          subtypes.filterNot(_.intermediate).map(_.child.name) ++ reached.flatMap(_._2).flatten
        all
          .diff(all.distinct)
          .headOption
          .map(t => s"its subtype ${t.value} extends it more than once")
      }
    if (family.summands.isLeft) family else why.fold(family)(w => family.copy(summands = Left(w)))
  }

  // The summands a sealed intermediate stands for, through its own intermediates; none when it, or
  // one below it, is not summed.
  private def leaves(
      key: TypeName,
      found: Map[TypeName, Option[List[Found]]],
      seen: Set[TypeName]
  ): Option[List[TypeName]] =
    found.get(key).flatten.filter(_ => !seen(key)).flatMap { subtypes =>
      Traversals
        .defined(subtypes.map { f =>
          if (f.intermediate) leaves(f.child.name, found, seen + key) else Some(List(f.child.name))
        })
        .map(_.flatten)
    }

  // The definitions of a statement list, and of the objects nested in it at any depth, whose
  // `extends` clause names the parent plainly — each with the key it is solved under.
  private def candidates(level: Level, parent: String): List[(Defn, TypeName)] = {
    val here = level.stats.collect {
      case d: (Defn.Class | Defn.Trait | Defn.Object | Defn.Enum)
          if d.templ.inits.exists(i => SealedSlots.plainCallee(i.tpe).contains(parent)) =>
        (d: Defn) -> level.key(Walk.named(d))
    }
    here ++ level.modules.flatMap(m => candidates(Level(m.stats, m.key, m.nested), parent))
  }

  // Every template and enum case the parent's file writes whose `extends` clause names the parent
  // (or an alias that may stand for it), plainly, applied or qualified: the file holds every
  // subtype a sealed parent has.
  private def written(parent: Tree, names: Set[String]): List[Tree] =
    Trees.root(parent).collect {
      case t: Template if t.inits.exists(init => names.exists(named(init.tpe, _))) =>
        t.parent.getOrElse(t)
      case c: Defn.EnumCase if c.inits.exists(init => names.exists(named(init.tpe, _))) => c
    }

  // Whether a parent type is spelled with this name as its last segment, applied or not.
  private def named(tpe: Type, name: String): Boolean = tpe match {
    case Type.Apply.After_4_6_0(callee, _) => named(callee, name)
    case Type.Name(n)                      => n == name
    case Type.Select(_, Type.Name(n))      => n == name
    case _                                 => false
  }

  // Why a subtype the file writes is not one the sum reads.
  private def describe(
      tree: Tree,
      parent: String,
      status: Denotes,
      renamed: Set[String]
  ): String = tree match {
    case _: Term.NewAnonymous => "an anonymous instance in its file"
    case _: Defn.Given        => "a given instance in its file"
    case c: Defn.EnumCase     => s"an enum case extends it: ${c.name.value}"
    case d: Defn if inits(d).forall(init => !named(init.tpe, parent)) =>
      val through =
        if (inits(d).exists(init => renamed.exists(named(init.tpe, _)))) "a renamed import"
        else "an alias"
      s"a subtype extends it through $through: ${Walk.named(d).value}"
    case d: Defn
        if inits(d)
          .exists(init => SealedSlots.plainCallee(init.tpe).isEmpty && named(init.tpe, parent)) =>
      s"a subtype extends it through a qualified name: ${Walk.named(d).value}"
    case d: Defn if status == Denotes.Unknown =>
      s"a subtype whose clause may name another $parent: ${Walk.named(d).value}"
    case d: Defn       => s"a subtype outside its body and objects: ${Walk.named(d).value}"
    case p: Pkg.Object => s"a package object extends it: ${p.name.value}"
    case other         => s"a template the estimate does not read: ${other.productPrefix}"
  }

  private def inits(tree: Tree): List[Init] = tree match {
    case c: Defn.EnumCase => c.inits
    case other            => other.children.collect { case t: Template => t.inits }.flatten
  }

  // One subtype: a module counts its one value, a case or final class or an enum its own space, a
  // sealed intermediate its own sum — each a member of every instantiation of the parent. A class
  // open to extension, or an abstraction that is not sealed, has values no file declares.
  private def summand(
      d: Defn,
      key: TypeName,
      parent: SealedSlots.Parent,
      site: Stat => Option[Scope => Scope]
  ): Either[String, Found] = {
    def bound(
        s: Defn.Object | Defn.Class | Defn.Trait | Defn.Enum,
        intermediate: Boolean,
        library: List[(Defn, String)] = Nil
    ) =
      SealedSlots
        .instantiation(parent, SealedSlots.subtype(s, denoted(s)))
        .map { i =>
          // The view the subtype's own statement reads in; the family's where the system has none.
          def at(outside: Scope): Scope = site(s).fold(outside)(_(outside))
          val child = Child(
            key,
            module = s.isInstanceOf[Defn.Object],
            bindings = i.bindings.map((c, p) => TypeName.of(c) -> TypeName.of(p)),
            standard = outside =>
              unstandard(at(outside), "it", i)
                .map(why => s"a subtype $why: ${key.value}")
                .orElse(rebound(library, site, outside).map(open(key, _))),
            built = outside => Builds.unread(s, at(outside)),
          )
          Found(child, intermediate)
        }
    d match {
      case o: Defn.Object                              => bound(o, intermediate = false)
      case c: Defn.Class if Counter.isConcreteClass(c) =>
        if (c.mods.exists(_.is[Mod.Final])) bound(c, intermediate = false)
        else if (c.mods.exists(_.is[Mod.Case]))
          caseLeaf(c, key).flatMap(library => bound(c, intermediate = false, library))
        else Left(s"a subtype open to extension: ${key.value}")
      case e: Defn.Enum                                        => bound(e, intermediate = false)
      case t: (Defn.Class | Defn.Trait) if Counter.isSealed(t) => bound(t, intermediate = true)
      case _ => Left(s"an abstract subtype open to extension: ${key.value}")
    }
  }

  private def open(key: TypeName, why: String): String =
    s"a subtype open to extension: ${key.value} ($why)"

  // A case class subtype no `final` closes: open when a subclass may override a member it has
  // (`Overrides.openLeaf`); closed otherwise, with the library parents of its ancestry that have
  // nothing to override, which `rebound` checks where the system reads each clause.
  private def caseLeaf(c: Defn.Class, key: TypeName): Either[String, List[(Defn, String)]] = {
    val parents = new FileParents(c)
    val ancestry = Overrides.ancestry[Defn](c, parents)
    Overrides
      .openLeaf(c, c, ancestry, parents)
      .map(open(key, _))
      .toLeft(ancestry.collect {
        case Overrides.Clause(owner, name, Overrides.Parent.Library(listed)) if listed.isEmpty =>
          owner -> name
      })
  }

  // A library parent read as the stdlib's with nothing to override (`Product`) is the stdlib's only
  // where no source binds its name at the clause naming it — an import, a declaration of the
  // package in another file. Where the clause's view is not one the system reads, it may be.
  private def rebound(
      library: List[(Defn, String)],
      site: Stat => Option[Scope => Scope],
      outside: Scope
  ): Option[String] =
    library.collectFirst {
      case (owner, name) if site(owner).forall(_(outside).binds(TypeName.of(name))) =>
        s"parent not read: $name"
    }

  /** The constructor a subtype's field names: the declaration of the file that name denotes where
    * the subtype `d` is written (`ParentNames`), if any.
    */
  def denoted(d: Tree): String => Option[List[Variance]] =
    SealedSlots.declaredIn(
      d,
      (declared, name) => ParentNames.denotes(d, declared, name) == Denotes.Parent
    )

  /** Why a subtype is no member of every instantiation of `parent` where its clause is read
    * (`site`): a bottom pin, or a data constructor a field holds a parameter through, that is not
    * the standard library's there — a source binds the name (`type Nothing = Boolean`).
    */
  def unstandard(site: Scope, parent: String, i: SealedSlots.Instantiation): Option[String] = {
    def bound(name: String) = site.binds(TypeName.of(name))
    i.pins
      .find(bound)
      .map(pin => s"pins $parent with $pin, which a source binds")
      .orElse(
        i.data
          .find(bound)
          .map(c => s"holds a parameter of $parent through $c, which a source binds")
      )
  }

  // Invariant guard, which no fixture reaches: a subtype the reading cannot find is never an empty
  // summand, or the sum would fall short. Every subtype is solved in the system its family is (its
  // key is the family's sibling), and the family's equation reads only at its home scope.
  private def unread(sc: Scope, child: Child): Size = {
    sc.note(Definition.Unbound.Refusal(s"sealed subtype not read: ${child.name.value}"))
    EffectiveOmega
  }

  /** A summed family's equation: the sum of its subtypes. A module adds its one value; a subtype
    * with no parameter its solved value, with the reasons it rests on while a row collects them;
    * any other is read with each of its parameters bound to what the parent parameter its slot
    * binds is worth here — a parameter the parent's reader has not fixed stays the parent's reason,
    * and the subtype's reference to the parent at its own parameters is the parent's recursion.
    */
  def equation(family: Family, children: List[Child]): Named = {
    val parent = family.parent
    val params = Counter.parameterNames(parent)
    Named(
      params,
      outside => {
        val sc = Counter.signature(parent)(outside)
        children.foldLeft(NothingSize: Size) { (acc, child) =>
          acc + unmatched(outside, parent, child).getOrElse {
            // A summand whose construction provably builds nothing contributes no value: the sum
            // drops it, and no overcount is noted - the value may be one of the summands left.
            // Any other unread construction may hide a value built in the summand's own scope,
            // and keeps the row undecided.
            if (child.built(outside).exists(Builds.buildsNothing)) NothingSize
            else {
              unbuilt(outside, child)
              summand(sc, params, child)
            }
          }
        }
      },
      children = children
    )
  }

  // `Nothing` and `Any` pin a slot bottom-up, and a standard data constructor holds a parameter
  // as data, only as the standard library's: where the subtype's clause is written a source may
  // bind the name, and then the subtype is a member of some instantiations only.
  private def unmatched(outside: Scope, parent: Defn, child: Child): Option[Size] =
    child.standard(outside).map { reason =>
      outside.noteUnsummed(Walk.named(parent), reason)
      EffectiveOmega
    }

  // A module's one value counts whether or not it is built; a value row asks (`Builds`) — a class
  // child's equation notes it itself.
  private def unbuilt(outside: Scope, child: Child): Unit =
    if (child.module && outside.readingValue)
      child.built(outside).foreach(why => outside.note(Overcount.Unread(why)))

  private def summand(sc: Scope, params: List[TypeName], child: Child): Size =
    if (child.module) UnitSize
    else if (child.bindings.isEmpty) sc.solved(sc, child.name).getOrElse(unread(sc, child))
    else
      sc.definition(child.name).fold(unread(sc, child)) { named =>
        val frame = child.bindings.map((c, p) => c -> Counter.typeIn(sc)(Type.Name(p.value))).toMap
        val placeholders = child.bindings.collect { case (c, p) if sc.unfixed(p) => c }.toSet
        val arguments = child.bindings.flatMap((c, p) => sc.argument(p).map(c -> _)).toMap
        val own = params.map(p => child.bindings.collectFirst { case (c, `p`) => c })
        val summing = Scope.selfLens.get(sc).map(self => Itself(self.named, own)).toList
        val read = sc.origin(child.name, named).instantiating(frame, placeholders, arguments)
        named.equation(Scope.summingLens.modify(summing ++ _)(read))
      }

}
