package javamissing;

/** A class the `javafixtures` package is compiled against but not packaged with. */
public abstract class Gone {
  public boolean same(boolean b) {
    return !b;
  }
}
