package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.TypeEntry
import cardinality.types.Trees.hasOpaque

/** What a resolved type name denotes. The identity keys (`TypeIdentities`), a receiver's class
  * (`BodyTerms`) and an implicit scope's anchors (`Conversions`) classify a name the same way; each
  * keeps its own reading of each case.
  */
private[cardinality] enum Denotation {

  /** A supplied declaration other than a type alias or member: a class, trait, object or enum. */
  case Source(entry: TypeEntry)

  /** An opaque type, with its representation. */
  case Opaque(entry: TypeEntry, declaration: Defn.Type)

  /** A transparent alias: its body, read where it is declared. */
  case Alias(entry: TypeEntry, declaration: Defn.Type)

  /** An abstract type member. */
  case Abstract(entry: TypeEntry)

  /** A name nothing supplied binds: what it spells is the builtins' to say. */
  case Unbound

  /** A name an import binds to a path outside the sources. */
  case Foreign(path: String, reason: String)

  /** A name the lookup refused to read past. */
  case Blocked(reason: String)
}

private[cardinality] object Denotation {

  def of(found: Found): Denotation = found match {
    case Found.Entry(entry) =>
      entry.tree match {
        case d: Defn.Type if d.mods.hasOpaque => Opaque(entry, d)
        case d: Defn.Type                     => Alias(entry, d)
        case _: Decl.Type                     => Abstract(entry)
        case _                                => Source(entry)
      }
    case foreign: Found.Foreign => Foreign(foreign.path, foreign.reason)
    case blocked: Found.Blocked => Blocked(blocked.reason)
    case Found.Absent           => Unbound
  }

}
