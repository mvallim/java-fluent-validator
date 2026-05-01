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
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Utility class containing predicates for validating and comparing time strings.
 * All methods require a time pattern to parse string representations of times.
 */
public final class TimePredicate {

  /**
   * Checks if the time extracted from the source is between the minimum and maximum times (inclusive).
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param timeStringMin the minimum time as a string
   * @param timeStringMax the maximum time as a string
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is between the specified times (inclusive)
   */
  public static <T> Predicate<T> timeBetween(final Function<T, String> source, final String timeStringMin, final String timeStringMax, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeBetween(timeStringMin, timeStringMax, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the input time string is between the minimum and maximum times (inclusive).
   *
   * @param timeStringMin the minimum time as a string
   * @param timeStringMax the maximum time as a string
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the input time string is between the specified times (inclusive)
   */
  public static Predicate<String> timeBetween(final String timeStringMin, final String timeStringMax, final String pattern) {
    return PredicateBuilder.<String>from(timeLessThanOrEqual(timeStringMax, pattern)
        .and(timeGreaterThanOrEqual(timeStringMin, pattern)));
  }

  /**
   * Checks if the time extracted from the source is equal to the target time extracted from the same object.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the function to extract the target time string from the input object
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted times are equal
   */
  public static <T> Predicate<T> timeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeEqualTo(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the time extracted from the source is equal to a fixed target time string.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the target time as a string to compare against
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is equal to the target time
   */
  public static <T> Predicate<T> timeEqualTo(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeEqualTo(target, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the input time string is equal to a target time string.
   *
   * @param timeString the target time as a string to compare against
   * @param pattern    the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the input time string is equal to the target time string
   */
  public static Predicate<String> timeEqualTo(final String timeString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(timeEqualTo -> not(stringEmptyOrNull()).test(timeString))
        .and(timeEqualTo -> not(stringEmptyOrNull()).test(pattern)).and(timeEqualTo -> {
          try {
            final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern(pattern);
            final LocalTime timeTest = LocalTime.parse(timeEqualTo, timeFormat);
            final LocalTime time = LocalTime.parse(timeString, timeFormat);
            return timeTest.equals(time);
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   * Checks if the time extracted from the source is greater than the target time extracted from the same object.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the function to extract the target time string from the input object
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is greater than the target time
   */
  public static <T> Predicate<T> timeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeGreaterThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the time extracted from the source is greater than a fixed target time string.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the target time as a string to compare against
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is greater than the target time
   */
  public static <T> Predicate<T> timeGreaterThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeGreaterThan(target, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the input time string is greater than a target time string.
   *
   * @param timeString the target time as a string to compare against
   * @param pattern    the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the input time string is greater than the target time string
   */
  public static Predicate<String> timeGreaterThan(final String timeString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(timeGreaterThan -> not(stringEmptyOrNull()).test(timeString))
        .and(timeGreaterThan -> not(stringEmptyOrNull()).test(pattern)).and(timeGreaterThan -> {
          try {
            final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern(pattern);
            final LocalTime timeTest = LocalTime.parse(timeGreaterThan, timeFormat);
            final LocalTime time = LocalTime.parse(timeString, timeFormat);
            return timeTest.isAfter(time);
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   * Checks if the time extracted from the source is greater than or equal to the target time extracted from the same object.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the function to extract the target time string from the input object
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is greater than or equal to the target time
   */
  public static <T> Predicate<T> timeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder
        .<T>from(timeGreaterThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
  }

  /**
   * Checks if the time extracted from the source is greater than or equal to a fixed target time string.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the target time as a string to compare against
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is greater than or equal to the target time
   */
  public static <T> Predicate<T> timeGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder
        .<T>from(timeGreaterThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
  }

  /**
   * Checks if the input time string is greater than or equal to a target time string.
   *
   * @param timeString the target time as a string to compare against
   * @param pattern    the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the input time string is greater than or equal to the target time string
   */
  public static Predicate<String> timeGreaterThanOrEqual(final String timeString, final String pattern) {
    return PredicateBuilder
        .<String>from(timeGreaterThan(timeString, pattern).or(timeEqualTo(timeString, pattern)));
  }

  /**
   * Checks if the time extracted from the source is less than the target time extracted from the same object.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the function to extract the target time string from the input object
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is less than the target time
   */
  public static <T> Predicate<T> timeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeLessThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the time extracted from the source is less than a fixed target time string.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the target time as a string to compare against
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is less than the target time
   */
  public static <T> Predicate<T> timeLessThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> timeLessThan(target, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the input time string is less than a target time string.
   *
   * @param timeString the target time as a string to compare against
   * @param pattern    the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the input time string is less than the target time string
   */
  public static Predicate<String> timeLessThan(final String timeString, final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(timeLessThan -> not(stringEmptyOrNull()).test(timeString))
        .and(timeLessThan -> not(stringEmptyOrNull()).test(pattern)).and(timeLessThan -> {
          try {
            final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern(pattern);
            final LocalTime timeTest = LocalTime.parse(timeLessThan, timeFormat);
            final LocalTime time = LocalTime.parse(timeString, timeFormat);
            return timeTest.isBefore(time);
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   * Checks if the time extracted from the source is less than or equal to the target time extracted from the same object.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the function to extract the target time string from the input object
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is less than or equal to the target time
   */
  public static <T> Predicate<T> timeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder
        .<T>from(timeLessThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
  }

  /**
   * Checks if the time extracted from the source is less than or equal to a fixed target time string.
   *
   * @param <T>         the type of the input object
   * @param source      the function to extract the time string from the input object
   * @param target      the target time as a string to compare against
   * @param pattern     the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the extracted time is less than or equal to the target time
   */
  public static <T> Predicate<T> timeLessThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder
        .<T>from(timeLessThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
  }

  /**
   * Checks if the input time string is less than or equal to a target time string.
   *
   * @param timeString the target time as a string to compare against
   * @param pattern    the time pattern to parse the time strings
   * @return a predicate that evaluates to true if the input time string is less than or equal to the target time string
   */
  public static Predicate<String> timeLessThanOrEqual(final String timeString, final String pattern) {
    return PredicateBuilder
        .<String>from(timeLessThan(timeString, pattern).or(timeEqualTo(timeString, pattern)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private TimePredicate() {
    super();
  }

}
