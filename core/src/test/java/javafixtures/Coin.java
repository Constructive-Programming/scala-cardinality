package javafixtures;

/** A Java enum: exactly its two constants. A static field of the enum's type is no constant. */
public enum Coin {
  HEADS,
  TAILS;

  public static final Coin DEFAULT = HEADS;

  public boolean heads() {
    return this == HEADS;
  }
}
