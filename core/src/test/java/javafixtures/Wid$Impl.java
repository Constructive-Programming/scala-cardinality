package javafixtures;

/** A top-level class whose name holds a `$`: no class `Wid` exists to be its outer class. */
public class Wid$Impl {
  public int f() {
    return 1;
  }

  public static Wid$Impl made() {
    return new Wid$Impl() {
      @Override
      public int f() {
        return 2;
      }
    };
  }
}
