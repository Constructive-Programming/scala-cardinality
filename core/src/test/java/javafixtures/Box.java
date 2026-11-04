package javafixtures;

import java.util.List;

/** A generic Java class with state its constructor sets, a wildcard, a raw type and nested types. */
public class Box<A> {
  private final A value;

  public Box(A value) {
    this.value = value;
  }

  public A get() {
    return value;
  }

  public static <B> Box<B> of(B b) {
    return new Box<>(b);
  }

  public List<? extends A> wild() {
    return List.of(value);
  }

  @SuppressWarnings("rawtypes")
  public List raw() {
    return List.of(value);
  }

  public Object any(Object o) {
    return o;
  }

  /** A static nested enum: `Box.Size`. */
  public enum Size {
    SMALL,
    LARGE
  }

  /** A final class whose one field its constructor sets. */
  public static final class Point {
    public final boolean x;

    public Point(boolean x) {
      this.x = x;
    }
  }
}
