package javafixtures;

/** A Java class that is not written final and that no named class extends. */
public class Lone {
  public boolean on() {
    return true;
  }

  /** An anonymous subclass, which overrides `on`. */
  public static Lone off() {
    return new Lone() {
      @Override
      public boolean on() {
        return false;
      }
    };
  }
}
