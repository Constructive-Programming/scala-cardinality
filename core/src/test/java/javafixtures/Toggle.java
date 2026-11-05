package javafixtures;

/** A Java interface: an abstract method, a default one, a static one and a constant. */
public interface Toggle {
  int LIMIT = 3;

  boolean flip(boolean b);

  default boolean same(boolean b) {
    return b;
  }

  static Toggle identity() {
    return b -> b;
  }
}
