package javafixtures;

/** A sealed Java interface: its permitted subclasses are not read. */
public sealed interface Choice permits Choice.Yes, Choice.No {
  boolean yes();

  /** One permitted subclass. */
  final class Yes implements Choice {
    public boolean yes() {
      return true;
    }
  }

  /** The other. */
  final class No implements Choice {
    public boolean yes() {
      return false;
    }
  }
}
