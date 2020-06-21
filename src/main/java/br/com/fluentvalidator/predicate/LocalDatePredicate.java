package br.com.fluentvalidator.predicate;

import java.time.LocalDate;
import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

public final class LocalDatePredicate {

  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateAfterToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDate -> localDate.isAfter(LocalDate.now()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateAfterToday(final Function<T, LocalDate> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> localDateAfterToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBeforeToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDate -> localDate.isBefore(LocalDate.now()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBeforeToday(final Function<T, LocalDate> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> localDateBeforeToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateIsToday(){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> obj.isEqual(LocalDate.now()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateIsToday(final Function<T, LocalDate> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateIsToday().test(source.apply(obj)));
  }


  /**
   *
   * @param localDate
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateEqualTo(final LocalDate localDate){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(localDate))
        .and(localDate::isEqual);
  }

  /**
   *
   * @param source
   * @param localDate
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateEqualTo(final Function<T, LocalDate> source, final LocalDate localDate){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateEqualTo(localDate).test(source.apply(obj)));
  }


  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateAfter(final Function<T, LocalDate> source, final Function<T, LocalDate> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> localDateAfter(target.apply(obj)).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateAfter(final Function<T, LocalDate> source, final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateAfter(target).test(source.apply(obj)));
  }

  /**
   *
   * @param target
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateAfter(final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isAfter(target));
  }

  /**
   *
   * @param target
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateAfterInclusive(final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localDateAfter(target)).or(localDateEqualTo(target)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateAfterInclusive(final Function<T, LocalDate> source, final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateAfterInclusive(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateAfterInclusive(final Function<T, LocalDate> source, final Function<T, LocalDate> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> localDateAfterInclusive(target.apply(obj)).test(source.apply(obj)));
  }


  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBefore(final Function<T, LocalDate> source, final Function<T, LocalDate> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> localDateBefore(target.apply(obj)).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBefore(final Function<T, LocalDate> source, final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateBefore(target).test(source.apply(obj)));
  }

  /**
   *
   * @param target
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBeforeInclusive(final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localDateBefore(target)).or(localDateEqualTo(target)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBeforeInclusive(final Function<T, LocalDate> source, final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateBeforeInclusive(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBeforeInclusive(final Function<T, LocalDate> source, final Function<T, LocalDate> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> localDateBeforeInclusive(target.apply(obj)).test(source.apply(obj)));
  }


  /**
   *
   * @param target
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBefore(final LocalDate target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isBefore(target));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetween(final LocalDate min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateAfter(min).and(localDateBefore(max)));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetween(final Function<T, LocalDate> min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localDateBetween(min.apply(obj), max).test(obj));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetween(final LocalDate min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetween(min, max.apply(obj)).test(obj));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetween(final Function<T, LocalDate> min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetween(min.apply(obj), max.apply(obj)).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetween(final Function<T, LocalDate> source, final LocalDate min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateBetween(min, max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetween(final Function<T, LocalDate> source, final Function<T, LocalDate> min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(min)))
        .and(obj -> localDateBetween(min.apply(obj), max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetween(final Function<T, LocalDate> source, final LocalDate min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetween(min, max.apply(obj)).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetween(final Function<T, LocalDate> source, final Function<T, LocalDate> min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetween(min.apply(obj), max.apply(obj)).test(source.apply(obj)));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetweenInclusive(final LocalDate min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateAfterInclusive(min).and(localDateBeforeInclusive(max)));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetweenInclusive(final Function<T, LocalDate> min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localDateBetweenInclusive(min.apply(obj), max).test(obj));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetweenInclusive(final LocalDate min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetweenInclusive(min, max.apply(obj)).test(obj));
  }

  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDate> Predicate<T> localDateBetweenInclusive(final Function<T, LocalDate> min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetweenInclusive(min.apply(obj), max.apply(obj)).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetweenInclusive(final Function<T, LocalDate> source, final LocalDate min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateBetweenInclusive(min, max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetweenInclusive(final Function<T, LocalDate> source, final Function<T, LocalDate> min, final LocalDate max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(min)))
        .and(obj -> localDateBetweenInclusive(min.apply(obj), max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetweenInclusive(final Function<T, LocalDate> source, final LocalDate min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetweenInclusive(min, max.apply(obj)).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateBetweenInclusive(final Function<T, LocalDate> source, final Function<T, LocalDate> min, final Function<T, LocalDate> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateBetweenInclusive(min.apply(obj), max.apply(obj)).test(source.apply(obj)));
  }


  private LocalDatePredicate() {
    super();
  }

}
