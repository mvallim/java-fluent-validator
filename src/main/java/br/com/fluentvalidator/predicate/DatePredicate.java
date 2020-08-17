package br.com.fluentvalidator.predicate;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

public final class DatePredicate {

  /**
   *
   * @param <T>
   * @param source
   * @param dateStringMin
   * @param dateStringMax
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateBetween(final Function<T, String> source, final String dateStringMin, final String dateStringMax, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateBetween(dateStringMin, dateStringMax, pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param dateStringMin
   * @param dateStringMax
   * @param pattern
   * @return
   */
  public static Predicate<String> dateBetween(final String dateStringMin, final String dateStringMax, final String pattern) {
    return PredicateBuilder.<String>from(dateLessThanOrEqual(dateStringMax, pattern)
        .and(dateGreaterThanOrEqual(dateStringMin, pattern)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateEqualTo(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateEqualTo(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateEqualTo(target, pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param dateString
   * @param pattern
   * @return
   */
  public static Predicate<String> dateEqualTo(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(dateEqualTo -> not(stringEmptyOrNull()).test(dateString))
        .and(dateEqualTo -> not(stringEmptyOrNull()).test(pattern)).and(dateEqualTo -> {
          try {
            final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern);
            final LocalDate dateTest = LocalDate.parse(dateEqualTo, dateFormat);
            final LocalDate date = LocalDate.parse(dateString, dateFormat);
            return dateTest.isEqual(date);
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateGreaterThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateGreaterThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateGreaterThan(target, pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param dateString
   * @param pattern
   * @return
   */
  public static Predicate<String> dateGreaterThan(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(dateGreaterThan -> not(stringEmptyOrNull()).test(dateString))
        .and(dateGreaterThan -> not(stringEmptyOrNull()).test(pattern)).and(dateGreaterThan -> {
          try {
            final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern);
            final LocalDate dateTest = LocalDate.parse(dateGreaterThan, dateFormat);
            final LocalDate date = LocalDate.parse(dateString, dateFormat);
            return dateTest.isAfter(date);
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder
        .<T>from(dateGreaterThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder
        .<T>from(dateGreaterThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   *
   * @param dateString
   * @param pattern
   * @return
   */
  public static Predicate<String> dateGreaterThanOrEqual(final String dateString, final String pattern) {
    return PredicateBuilder
        .<String>from(dateGreaterThan(dateString, pattern).or(dateEqualTo(dateString, pattern)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateLessThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateLessThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> dateLessThan(target, pattern).test(source.apply(obj)));
  }

  /**
   *
   * @param dateString
   * @param pattern
   * @return
   */
  public static Predicate<String> dateLessThan(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(dateLessThan -> not(stringEmptyOrNull()).test(dateString))
        .and(dateLessThan -> not(stringEmptyOrNull()).test(pattern)).and(dateLessThan -> {
          try {
            final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern);
            final LocalDate dateTest = LocalDate.parse(dateLessThan, dateFormat);
            final LocalDate date = LocalDate.parse(dateString, dateFormat);
            return dateTest.isBefore(date);
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder
        .<T>from(dateLessThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param target
   * @param pattern
   * @return
   */
  public static <T> Predicate<T> dateLessThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder
        .<T>from(dateLessThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   *
   * @param dateString
   * @param pattern
   * @return
   */
  public static Predicate<String> dateLessThanOrEqual(final String dateString, final String pattern) {
    return PredicateBuilder
        .<String>from(dateLessThan(dateString, pattern).or(dateEqualTo(dateString, pattern)));
  }


  private DatePredicate() {
    super();
  }

}
