package javafixtures;

/** A Java interface with a default method, implemented only beside a parent outside the program. */
public interface Dial {
  default boolean same(boolean b) {
    return b;
  }
}
