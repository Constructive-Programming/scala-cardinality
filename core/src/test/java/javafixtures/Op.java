package javafixtures;

/** A Java enum whose constants have bodies: still exactly its constants. */
public enum Op {
  KEEP {
    public boolean apply(boolean b) {
      return b;
    }
  },
  FLIP {
    public boolean apply(boolean b) {
      return !b;
    }
  };

  public abstract boolean apply(boolean b);
}
