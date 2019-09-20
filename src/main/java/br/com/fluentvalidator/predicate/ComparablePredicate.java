package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Predicate;

public final class ComparablePredicate {

  private static final Integer MINUS = -1;

  private static final Integer PLUS = 1;

  private static final Integer ZERO = 0;

  private ComparablePredicate() {
    super();
  }

  public static <E, T extends Comparable<E>> Predicate<T> equalTo(final E value) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(equalTo -> ZERO.equals(equalTo.compareTo(value)));
  }

  public static <E, T extends Comparable<E>> Predicate<T> lessThan(final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(lessThan -> MINUS.equals(lessThan.compareTo(max)));
  }

  public static <E, T extends Comparable<E>> Predicate<T> greaterThan(final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(greaterThan -> PLUS.equals(greaterThan.compareTo(min)));
  }

  public static <E, T extends Comparable<E>> Predicate<T> greaterThanOrEqual(final E min) {
    return PredicateBuilder.<T>from(not(nullValue())).and(greaterThan(min).or(equalTo(min)));
  }

  public static <E, T extends Comparable<E>> Predicate<T> lessThanOrEqual(final E max) {
    return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).or(equalTo(max)));
  }

  public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).and(greaterThan(min)));
  }

}
