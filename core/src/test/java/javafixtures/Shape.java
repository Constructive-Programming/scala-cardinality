package javafixtures;

/** An abstract Java class extending another Java class. */
public abstract class Shape extends Box<Boolean> {
  protected Shape() {
    super(true);
  }

  public abstract boolean round();
}
