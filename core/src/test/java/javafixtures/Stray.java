package javafixtures;

/** Classes whose parent, `javamissing.Gone`, is on no classpath the fixtures read. */
public class Stray {
  public static Object outside() {
    return new javamissing.Gone() {};
  }

  public static Dial local() {
    class Local extends javamissing.Gone implements Dial {}
    return new Local();
  }
}
