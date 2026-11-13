package javafixtures;

/** A class extended through a specialization only, by an anonymous class. */
public class Gen {
  public boolean on() {
    return true;
  }

  public boolean off() {
    return false;
  }

  public static Gen made() {
    return new Gen$mcI$sp() {};
  }
}
