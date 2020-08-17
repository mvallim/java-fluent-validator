package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.function.FunctionBuilder.of;

public final class ComparablePredicate {

  private static final Integer ZERO = 0;
  
  /**
   *
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(lessThan(max).and(greaterThan(min)));
  }

  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(between(of((T fn) -> min), max));
  }
  
  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(between(min, of((T fn) -> max)));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> between(min, max).test(source.apply(obj)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(between(source, of((T fn) -> min), max));
  }

  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> lessThan(max.apply(obj)).and(greaterThan(min.apply(obj))).test(obj));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(between(source, min, of((T fn) -> max)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> lessThan(max.apply(obj)).and(greaterThan(min.apply(obj))).test(source.apply(obj)));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(lessThanOrEqual(max).and(greaterThanOrEqual(min)));
  }

  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final E min, final Function<T, E>  max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(betweenInclusive(of((T fn) -> min), max));
  }

  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E>  min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(betweenInclusive(min, of((T fn) -> max)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(betweenInclusive(source, of((T fn) -> min), of((T fn) -> max)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(betweenInclusive(source, of((T fn) -> min), max));
  }

  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E>  min, final Function<T, E>  max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> lessThanOrEqual(max.apply(obj)).and(greaterThanOrEqual(min.apply(obj))).test(obj));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(betweenInclusive(source, min, of((T fn) -> max)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> betweenInclusive(min.apply(obj), max.apply(obj)).test(source.apply(obj)));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param value
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> equalTo(final E value) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(fn -> value)))
        .and(obj -> obj.compareTo(value) == ZERO);
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param value
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> equalTo(final Function<T, E> source, final E value) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(equalTo(source, of(fn -> value)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param target
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> equalTo(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> source.apply(obj).compareTo(target.apply(obj)) == ZERO);
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param min
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> greaterThan(final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(fn -> min)))
        .and(obj -> obj.compareTo(min) > ZERO);
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThan(final Function<T, E> source, final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(greaterThan(source, of(fn -> min)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param target
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThan(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> source.apply(obj).compareTo(target.apply(obj)) > ZERO);
  }
  
  /**
   *
   * @param <E>
   * @param <T>
   * @param min
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> greaterThanOrEqual(final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(greaterThan(min).or(equalTo(min)));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThanOrEqual(final Function<T, E> source, final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(greaterThanOrEqual(source, of(fn -> min)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param target
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThanOrEqual(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(greaterThan(source, target).or(equalTo(source, target)));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> lessThan(final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(fn -> max)))
        .and(lessThan -> lessThan.compareTo(max) < ZERO);
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThan(final Function<T, E> source, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(lessThan(source, of(fn -> max)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param target
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThan(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> source.apply(obj).compareTo(target.apply(obj)) < ZERO);
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param max
   * @return
   */
  public static <E, T extends Comparable<E>> Predicate<T> lessThanOrEqual(final E max) {
    return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).or(equalTo(max)));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param max
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThanOrEqual(final Function<T, E> source, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(lessThanOrEqual(source, of(fn -> max)));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param target
   * @return
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThanOrEqual(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(lessThan(source, target).or(equalTo(source, target)));
  }

  private ComparablePredicate() {
    super();
  }

}
