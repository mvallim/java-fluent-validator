package br.com.fluentvalidator.predicate;

import java.util.function.Predicate;

public final class LogicalPredicate {

  private LogicalPredicate() {
    super();
  }

  /**
   *
   * @param predicate
   * @return
   */
  static <T> Predicate<T> is(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.and(is -> true));
  }

  /**
   *
   * @param predicate
   * @return
   */
  public static <T> Predicate<T> not(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.negate());
  }

  /**
   *
   * @return
   */
  public static <T> Predicate<T> isTrue() {
    return PredicateBuilder.<T>from(isTrue -> true);
  }

  /**
   *
   * @return
   */
  public static <T> Predicate<T> isFalse() {
    return PredicateBuilder.<T>from(isFalse -> false);
  }

}
