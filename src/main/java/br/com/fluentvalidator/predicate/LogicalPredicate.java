package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.Predicate;

public final class LogicalPredicate {

  /**
   *
   * @param <T>
   * @param left
   * @param right
   * @return
   */
  public static <T> Predicate<T> and(final BooleanSupplier left, final BooleanSupplier right) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> left.getAsBoolean() && right.getAsBoolean());
  }

  /**
   *
   * @param <T>
   * @param left
   * @param right
   * @return
   */
  public static <T> Predicate<T> and(final Predicate<T> left, final Predicate<T> right) {
    return PredicateBuilder.<T>from(not(nullValue())).and(left.and(right));
  }

  /**
   *
   * @param <T>
   * @param left
   * @param right
   * @return
   */
  public static <T> Predicate<T> or(final BooleanSupplier left, final BooleanSupplier right) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> left.getAsBoolean() || right.getAsBoolean());
  }

  /**
   *
   * @param <T>
   * @param left
   * @param right
   * @return
   */
  public static <T> Predicate<T> or(final Predicate<T> left, final Predicate<T> right) {
    return PredicateBuilder.<T>from(not(nullValue())).and(left.or(right));
  }

  /**
   *
   * @param <T>
   * @param left
   * @param right
   * @return
   */
  public static <T> Predicate<T> xor(final BooleanSupplier left, final BooleanSupplier right) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> left.getAsBoolean() ^ right.getAsBoolean());
  }

  /**
   *
   * @param <T>
   * @param left
   * @param right
   * @return
   */
  public static <T> Predicate<T> xor(final Predicate<T> left, final Predicate<T> right) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> left.test(obj) ^ right.test(obj));
  }

  /**
   *
   * @param <T>
   * @param predicate
   * @return
   */
  static <T> Predicate<T> is(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.and(is -> true));
  }

  /**
   *
   * @return
   */
  public static Predicate<Boolean> isFalse() {
    return PredicateBuilder.<Boolean>from(not(nullValue())).and(not(isFalse -> isFalse));
  }

  /**
   *
   * @param <T>
   * @param function
   * @return
   */
  public static <T> Predicate<T> isFalse(final Function<T, Boolean> function) {
    return PredicateBuilder.<T>from(not(nullValue())).and(not(nullValue(function))).and(not(function::apply));
  }

  /**
   *
   * @return
   */
  public static Predicate<Boolean> isTrue() {
    return PredicateBuilder.<Boolean>from(not(nullValue())).and(is(isTrue -> isTrue));
  }

  /**
   *
   * @param <T>
   * @param function
   * @return
   */
  public static <T> Predicate<T> isTrue(final Function<T, Boolean> function) {
    return PredicateBuilder.<T>from(not(nullValue())).and(not(nullValue(function))).and(function::apply);
  }

  /**
   *
   * @param <T>
   * @param predicate
   * @return
   */
  public static <T> Predicate<T> not(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.negate());
  }

  private LogicalPredicate() {
    super();
  }

}
