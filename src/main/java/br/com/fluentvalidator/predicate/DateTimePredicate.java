/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Utility class providing predicates for date-time string comparisons.
 * <p>
 * This class contains static factory methods that create {@link java.util.function.Predicate} instances
 * for validating and comparing date-time strings using a specified pattern.
 * </p>
 */
public final class DateTimePredicate {

  /**
   * Creates a predicate that tests if a date/time value is between two date/time values.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object
   * @param dateStringMin the minimum date/time string
   * @param dateStringMax the maximum date/time string
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is between the two specified values
   */
  public static <T> Predicate<T> dateTimeBetween(final Function<T, String> source, final String dateStringMin, final String dateStringMax, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeBetween(dateStringMin, dateStringMax, pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is between two date/time values.
   *
   * @param dateStringMin the minimum date/time string
   * @param dateStringMax the maximum date/time string
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is between the two specified values
   */
  public static Predicate<String> dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern) {
    return PredicateBuilder.<String>from(dateTimeLessThanOrEqual(dateStringMax, pattern)
      .and(dateTimeGreaterThanOrEqual(dateStringMin, pattern)));
  }

  /**
   * Creates a predicate that tests if a date/time value is equal to another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the function to extract the date/time string from the object to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is equal to the other date/time value
   */
  public static <T> Predicate<T> dateTimeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeEqualTo(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is equal to another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is equal to the other date/time value
   */
  public static <T> Predicate<T> dateTimeEqualTo(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeEqualTo(target, pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is equal to the specified date/time.
   *
   * @param dateString the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is equal to the specified date/time
   */
  public static Predicate<String> dateTimeEqualTo(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
      .and(dateTimeEqualTo -> not(stringEmptyOrNull()).test(dateString))
      .and(dateTimeEqualTo -> not(stringEmptyOrNull()).test(pattern)).and(dateTimeEqualTo -> {
        try {
          final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern);
          final LocalDateTime dateTimeTest = LocalDateTime.parse(dateTimeEqualTo, dateFormat);
          final LocalDateTime dateTime = LocalDateTime.parse(dateString, dateFormat);
          return dateTimeTest.isEqual(dateTime);
        } catch (final IllegalArgumentException | DateTimeParseException ex) {
          return false;
        }
      });
  }

  /**
   * Creates a predicate that tests if a date/time value is greater than another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the function to extract the date/time string from the object to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is greater than the other date/time value
   */
  public static <T> Predicate<T> dateTimeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeGreaterThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is greater than another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is greater than the other date/time value
   */
  public static <T> Predicate<T> dateTimeGreaterThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeGreaterThan(target, pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is greater than the specified date/time.
   *
   * @param dateString the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is greater than the specified date/time
   */
  public static Predicate<String> dateTimeGreaterThan(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
      .and(dateTimeGreaterThan -> not(stringEmptyOrNull()).test(dateString))
      .and(dateTimeGreaterThan -> not(stringEmptyOrNull()).test(pattern))
      .and(dateTimeGreaterThan -> {
        try {
          final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern);
          final LocalDateTime dateTimeTest = LocalDateTime.parse(dateTimeGreaterThan, dateFormat);
          final LocalDateTime dateTime = LocalDateTime.parse(dateString, dateFormat);
          return dateTimeTest.isAfter(dateTime);
        } catch (final IllegalArgumentException | DateTimeParseException ex) {
          return false;
        }
      });
  }

  /**
   * Creates a predicate that tests if a date/time value is greater than or equal to another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the function to extract the date/time string from the object to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is greater than or equal to the other date/time value
   */
  public static <T> Predicate<T> dateTimeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(dateTimeGreaterThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
  }

  /**
   * Creates a predicate that tests if a date/time value is greater than or equal to another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is greater than or equal to the other date/time value
   */
  public static <T> Predicate<T> dateTimeGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(dateTimeGreaterThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
  }

  /**
   * Creates a predicate that tests if a date/time value is greater than or equal to the specified date/time.
   *
   * @param dateString the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is greater than or equal to the specified date/time
   */
  public static Predicate<String> dateTimeGreaterThanOrEqual(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(dateTimeGreaterThan(dateString, pattern).or(dateTimeEqualTo(dateString, pattern)));
  }

  /**
   * Creates a predicate that tests if a date/time value is less than another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the function to extract the date/time string from the object to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is less than the other date/time value
   */
  public static <T> Predicate<T> dateTimeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeLessThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is less than another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is less than the other date/time value
   */
  public static <T> Predicate<T> dateTimeLessThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateTimeLessThan(target, pattern).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a date/time value is less than the specified date/time.
   *
   * @param dateString the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is less than the specified date/time
   */
  public static Predicate<String> dateTimeLessThan(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
      .and(dateTimeLessThan -> not(stringEmptyOrNull()).test(dateString))
      .and(dateTimeLessThan -> not(stringEmptyOrNull()).test(pattern)).and(dateTimeLessThan -> {
        try {
          final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern);
          final LocalDateTime dateTest = LocalDateTime.parse(dateTimeLessThan, dateFormat);
          final LocalDateTime date = LocalDateTime.parse(dateString, dateFormat);
          return dateTest.isBefore(date);
        } catch (final IllegalArgumentException | DateTimeParseException ex) {
          return false;
        }
      });
  }

  /**
   * Creates a predicate that tests if a date/time value is less than or equal to another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the function to extract the date/time string from the object to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is less than or equal to the other date/time value
   */
  public static <T> Predicate<T> dateTimeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(dateTimeLessThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
  }

  /**
   * Creates a predicate that tests if a date/time value is less than or equal to another date/time value.
   *
   * @param <T> the type of the object to test
   * @param source the function to extract the date/time string from the object to test
   * @param target the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is less than or equal to the other date/time value
   */
  public static <T> Predicate<T> dateTimeLessThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(dateTimeLessThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
  }

  /**
   * Creates a predicate that tests if a date/time value is less than or equal to the specified date/time.
   *
   * @param dateString the date/time string to compare against
   * @param pattern the pattern to parse the date/time strings
   * @return a predicate that tests if the date/time value is less than or equal to the specified date/time
   */
  public static Predicate<String> dateTimeLessThanOrEqual(final String dateString, final String pattern) {
    return PredicateBuilder.<String>from(dateTimeLessThan(dateString, pattern).or(dateTimeEqualTo(dateString, pattern)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private DateTimePredicate() {
    super();
  }

}
