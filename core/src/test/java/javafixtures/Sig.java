package javafixtures;

import java.util.List;

/** Java interfaces of one abstract method: over `Object`, a wildcard, a raw type. */
public final class Sig {
  private Sig() {}

  /** `Object` in and out. */
  public interface AnyFn {
    Object apply(Object x);
  }

  /** A wildcard bounded by the interface's parameter. */
  public interface Wild<A> {
    List<? extends A> wild();
  }

  /** A raw type: `List` without its argument. */
  public interface Raw {
    @SuppressWarnings("rawtypes")
    List raw();
  }
}
