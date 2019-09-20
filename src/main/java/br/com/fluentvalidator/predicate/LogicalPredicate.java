package br.com.fluentvalidator.predicate;

import java.util.function.Predicate;

public final class LogicalPredicate {

  private LogicalPredicate() {
    super();
  }

  static <T> Predicate<T> is(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.and(is -> true));
  }

  public static <T> Predicate<T> not(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(is(predicate).negate());
  }

  public static <T> Predicate<T> isTrue() {
    return PredicateBuilder.<T>from(is(isTrue -> true));
  }

  public static <T> Predicate<T> isFalse() {
    return PredicateBuilder.<T>from(is(isFalse -> false));
  }

}
