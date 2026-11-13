package javarecords;

/** A Java interface whose default method a class overrides. */
public interface Dial {
  boolean turn(boolean b);

  default boolean twice(boolean b) {
    return turn(turn(b));
  }

  /** Overrides the default method. */
  final class Fast implements Dial {
    public boolean turn(boolean b) {
      return !b;
    }

    public boolean twice(boolean b) {
      return b;
    }
  }
}
