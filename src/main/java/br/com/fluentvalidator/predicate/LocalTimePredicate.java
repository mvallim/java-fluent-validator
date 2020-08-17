package br.com.fluentvalidator.predicate;

import java.time.LocalTime;
import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.predicate.LocalDatePredicate.*;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

public final class LocalTimePredicate {

  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeAfterNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTime -> localTimeAfter(LocalTime.now()).test(localTime));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeAfterNow(final Function<T, LocalTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeAfterNow().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeBeforeNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTime -> localTimeBefore(LocalTime.now()).test(localTime));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBeforeNow(final Function<T, LocalTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBeforeNow().test(source.apply(obj)));
  }


  /**
   *
   * @param localTime
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeEqualTo(final LocalTime localTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(localTime))
        .and(obj -> localTime.compareTo(obj) == 0);
  }

  /**
   *
   * @param source
   * @param localTime
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeEqualTo(final Function<T, LocalTime> source, final LocalTime localTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeEqualTo(localTime).test(source.apply(obj)));
  }


  /**
   *
   * @param target
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeAfter(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isAfter(target));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeAfter(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeAfter(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeAfter(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeAfter(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param target
   * @param <T>
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeAfterOrEqual(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localTimeAfter(target)).or(localTimeEqualTo(target)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeAfterOrEqual(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeAfterOrEqual(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeAfterOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeAfterOrEqual(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param target
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeBefore(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isBefore(target));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBefore(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBefore(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBefore(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeBefore(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param target
   * @param <T>
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeBeforeOrEqual(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localTimeBefore(target)).or(localTimeEqualTo(target)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBeforeOrEqual(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBeforeOrEqual(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBeforeOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeBeforeOrEqual(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeBetween(final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTimeAfter(min).and(localTimeBefore(max)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBetween(min, max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localTimeBetween(source, min.apply(obj), max).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final LocalTime min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetween(source, min, max.apply(obj)).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetween(source, min.apply(obj), max.apply(obj)).test(obj));
  }


  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalTime> Predicate<T> localTimeBetweenOrEqual(final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTimeAfterOrEqual(min).and(localTimeBeforeOrEqual(max)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBetweenOrEqual(min, max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localTimeBetweenOrEqual(source, min.apply(obj), max).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final LocalTime min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetweenOrEqual(source, min, max.apply(obj)).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetweenOrEqual(source, min.apply(obj), max.apply(obj)).test(obj));
  }


  private LocalTimePredicate() {
    super();
  }

}
