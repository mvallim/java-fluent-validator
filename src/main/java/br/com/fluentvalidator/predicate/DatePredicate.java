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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

/**
 * Utility class providing predicates for date string comparisons.
 * <p>
 * This class contains static factory methods that create {@link java.util.function.Predicate} instances
 * for validating and comparing date strings using a specified pattern.
 * </p>
 */
public final class DatePredicate {

  /**
   * Checks if the date from the source is between two dates (inclusive).
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param dateStringMin the minimum date as a string
   * @param dateStringMax the maximum date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is between the specified dates
   */
  public static <T> Predicate<T> dateBetween(final Function<T, String> source, final String dateStringMin, final String dateStringMax, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateBetween(dateStringMin, dateStringMax, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if a string is between two dates (inclusive).
   *
   * @param dateStringMin the minimum date as a string
   * @param dateStringMax the maximum date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the input date string is between the specified dates
   */
  public static Predicate<String> dateBetween(final String dateStringMin, final String dateStringMax, final String pattern) {
    return PredicateBuilder.<String>from(dateLessThanOrEqual(dateStringMax, pattern)
      .and(dateGreaterThanOrEqual(dateStringMin, pattern)));
  }

  /**
   * Checks if the date from the source is equal to the target date.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the function to extract the target date string from the object
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted dates are equal
   */
  public static <T> Predicate<T> dateEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateEqualTo(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the date from the source is equal to a target date string.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the target date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is equal to the target date
   */
  public static <T> Predicate<T> dateEqualTo(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateEqualTo(target, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if a string is equal to a target date string.
   *
   * @param dateString the date as a string
   * @param pattern    the date pattern
   * @return a predicate that evaluates if the input date string is equal to the target date string
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
   * Checks if the date from the source is greater than the target date.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the function to extract the target date string from the object
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is greater than the target date
   */
  public static <T> Predicate<T> dateGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateGreaterThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the date from the source is greater than a target date string.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the target date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is greater than the target date
   */
  public static <T> Predicate<T> dateGreaterThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateGreaterThan(target, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if a string is greater than a target date string.
   *
   * @param dateString the date as a string
   * @param pattern    the date pattern
   * @return a predicate that evaluates if the input date string is greater than the target date string
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
   * Checks if the date from the source is greater than or equal to the target date.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the function to extract the target date string from the object
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is greater than or equal to the target date
   */
  public static <T> Predicate<T> dateGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder
      .<T>from(dateGreaterThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   * Checks if the date from the source is greater than or equal to a target date string.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the target date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is greater than or equal to the target date
   */
  public static <T> Predicate<T> dateGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder
      .<T>from(dateGreaterThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   * Checks if a string is greater than or equal to a target date string.
   *
   * @param dateString the date as a string
   * @param pattern    the date pattern
   * @return a predicate that evaluates if the input date string is greater than or equal to the target date string
   */
  public static Predicate<String> dateGreaterThanOrEqual(final String dateString, final String pattern) {
    return PredicateBuilder
      .<String>from(dateGreaterThan(dateString, pattern).or(dateEqualTo(dateString, pattern)));
  }

  /**
   * Checks if the date from the source is less than the target date.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the function to extract the target date string from the object
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is less than the target date
   */
  public static <T> Predicate<T> dateLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateLessThan(target.apply(obj), pattern).test(source.apply(obj)));
  }

  /**
   * Checks if the date from the source is less than a target date string.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the target date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is less than the target date
   */
  public static <T> Predicate<T> dateLessThan(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> dateLessThan(target, pattern).test(source.apply(obj)));
  }

  /**
   * Checks if a string is less than a target date string.
   *
   * @param dateString the date as a string
   * @param pattern    the date pattern
   * @return a predicate that evaluates if the input date string is less than the target date string
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
   * Checks if the date from the source is less than or equal to the target date.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the function to extract the target date string from the object
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is less than or equal to the target date
   */
  public static <T> Predicate<T> dateLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
    return PredicateBuilder
      .<T>from(dateLessThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   * Checks if the date from the source is less than or equal to a target date string.
   *
   * @param <T>         the type of the object being evaluated
   * @param source      the function to extract the date string from the object
   * @param target      the target date as a string
   * @param pattern     the date pattern
   * @return a predicate that evaluates if the extracted date is less than or equal to the target date
   */
  public static <T> Predicate<T> dateLessThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
    return PredicateBuilder
      .<T>from(dateLessThan(source, target, pattern).or(dateEqualTo(source, target, pattern)));
  }

  /**
   * Checks if a string is less than or equal to a target date string.
   *
   * @param dateString the date as a string
   * @param pattern    the date pattern
   * @return a predicate that evaluates if the input date string is less than or equal to the target date string
   */
  public static Predicate<String> dateLessThanOrEqual(final String dateString, final String pattern) {
    return PredicateBuilder
      .<String>from(dateLessThan(dateString, pattern).or(dateEqualTo(dateString, pattern)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private DatePredicate() {
    super();
  }

}