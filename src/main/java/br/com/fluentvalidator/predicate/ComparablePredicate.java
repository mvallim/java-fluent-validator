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

import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.function.FunctionBuilder.of;

/**
 * Utility class providing predicates for comparing {@link Comparable} objects.
 * <p>
 * This class contains static factory methods that create {@link java.util.function.Predicate} instances
 * for various comparison operations, including greater than, less than, between, and equality checks.
 * </p>
 */
public final class ComparablePredicate {

  private static final Integer ZERO = 0;
  
  /**
   * Checks if the value is between two values (exclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the minimum value
   * @param max the maximum value
   * @return a predicate that evaluates if the input is between min and max
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(lessThan(max).and(greaterThan(min)));
  }

  /**
   * Checks if the value is between a minimum value and a maximum value provided by a function (exclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the minimum value
   * @param max the function that provides the maximum value
   * @return a predicate that evaluates if the input is between min and the provided maximum value
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(between(of((T fn) -> min), max));
  }
  
  /**
   * Checks if the value is between a minimum value provided by a function and a maximum value (exclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the function that provides the minimum value
   * @param max the maximum value
   * @return a predicate that evaluates if the input is between the provided minimum value and max
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(between(min, of((T fn) -> max)));
  }

  /**
   * Checks if the value extracted from the source is between two values (exclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the minimum value
   * @param max    the maximum value
   * @return a predicate that evaluates if the extracted value is between min and max
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> between(min, max).test(source.apply(obj)));
  }

  /**
   * Checks if the value extracted from the source is between a minimum value and a maximum value provided by a function (exclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the minimum value
   * @param max    the function that provides the maximum value
   * @return a predicate that evaluates if the extracted value is between min and the provided maximum value
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(between(source, of((T fn) -> min), max));
  }

  /**
   * Checks if the value extracted from the source is between two values provided by functions (exclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min  the function that provides the minimum value
   * @param max  the function that provides the maximum value
   * @return a predicate that evaluates if the input is between the provided minimum and maximum values
   */
  public static <E, T extends Comparable<E>> Predicate<T> between(final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(min)))
      .and(not(nullValue(max)))
      .and(obj -> lessThan(max.apply(obj)).and(greaterThan(min.apply(obj))).test(obj));
  }

  /**
   * Checks if the value extracted from the source is between a minimum value provided by a function and a maximum value (exclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the function that provides the minimum value
   * @param max    the maximum value
   * @return a predicate that evaluates if the extracted value is between the provided minimum value and max
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(between(source, min, of((T fn) -> max)));
  }

  /**
   * Checks if the value extracted from the source is between two values provided by functions (exclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the function that provides the minimum value
   * @param max    the function that provides the maximum value
   * @return a predicate that evaluates if the extracted value is between the provided minimum and maximum values
   */
  public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(min)))
      .and(not(nullValue(max)))
      .and(obj -> lessThan(max.apply(obj)).and(greaterThan(min.apply(obj))).test(source.apply(obj)));
  }

  /**
   * Checks if the value is between two values (inclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the minimum value
   * @param max the maximum value
   * @return a predicate that evaluates if the input is between or equal to min and max
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(lessThanOrEqual(max).and(greaterThanOrEqual(min)));
  }

  /**
   * Checks if the value is between a minimum value and a maximum value provided by a function (inclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the minimum value
   * @param max the function that provides the maximum value
   * @return a predicate that evaluates if the input is between or equal to min and the provided maximum value
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(of((T fn) -> min), max));
  }

  /**
   * Checks if the value is between a minimum value provided by a function and a maximum value (inclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the function that provides the minimum value
   * @param max the maximum value
   * @return a predicate that evaluates if the input is between or equal to the provided minimum value and max
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(min, of((T fn) -> max)));
  }

  /**
   * Checks if the value extracted from the source is between two values (inclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the minimum value
   * @param max    the maximum value
   * @return a predicate that evaluates if the extracted value is between or equal to min and max
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final E min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(source, of((T fn) -> min), of((T fn) -> max)));
  }

  /**
   * Checks if the value extracted from the source is between a minimum value and a maximum value provided by a function (inclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the minimum value
   * @param max    the function that provides the maximum value
   * @return a predicate that evaluates if the extracted value is between or equal to min and the provided maximum value
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final E min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(source, of((T fn) -> min), max));
  }

  /**
   * Checks if the value extracted from the source is between two values provided by functions (inclusive).
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min  the function that provides the minimum value
   * @param max  the function that provides the maximum value
   * @return a predicate that evaluates if the input is between or equal to the provided minimum and maximum values
   */
  public static <E, T extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(min)))
      .and(not(nullValue(max)))
      .and(obj -> lessThanOrEqual(max.apply(obj)).and(greaterThanOrEqual(min.apply(obj))).test(obj));
  }

  /**
   * Checks if the value extracted from the source is between a minimum value provided by a function and a maximum value (inclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the function that provides the minimum value
   * @param max    the maximum value
   * @return a predicate that evaluates if the extracted value is between or equal to the provided minimum value and max
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final Function<T, E> min, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(source, min, of((T fn) -> max)));
  }

  /**
   * Checks if the value extracted from the source is between two values provided by functions (inclusive).
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the function that provides the minimum value
   * @param max    the function that provides the maximum value
   * @return a predicate that evaluates if the extracted value is between or equal to the provided minimum and maximum values
   */
  public static <T, E extends Comparable<E>> Predicate<T> betweenInclusive(final Function<T, E> source, final Function<T, E> min, final Function<T, E> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(min)))
      .and(not(nullValue(max)))
      .and(obj -> betweenInclusive(min.apply(obj), max.apply(obj)).test(source.apply(obj)));
  }

  /**
   * Checks if the value is equal to another value.
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param value the value to compare against
   * @return a predicate that evaluates if the input is equal to the provided value
   */
  public static <E, T extends Comparable<E>> Predicate<T> equalTo(final E value) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(fn -> value)))
      .and(obj -> obj.compareTo(value) == ZERO);
  }

  /**
   * Checks if the value extracted from the source is equal to another value.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param value  the value to compare against
   * @return a predicate that evaluates if the extracted value is equal to the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> equalTo(final Function<T, E> source, final E value) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(equalTo(source, of(fn -> value)));
  }

  /**
   * Checks if the value extracted from the source is equal to another value provided by a function.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param target the function that provides the value to compare against
   * @return a predicate that evaluates if the extracted value is equal to the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> equalTo(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(target)))
      .and(obj -> source.apply(obj).compareTo(target.apply(obj)) == ZERO);
  }

  /**
   * Checks if the value is greater than another value.
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the value to compare against
   * @return a predicate that evaluates if the input is greater than the provided value
   */
  public static <E, T extends Comparable<E>> Predicate<T> greaterThan(final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(fn -> min)))
      .and(obj -> obj.compareTo(min) > ZERO);
  }

  /**
   * Checks if the value extracted from the source is greater than another value.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the value to compare against
   * @return a predicate that evaluates if the extracted value is greater than the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThan(final Function<T, E> source, final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(greaterThan(source, of(fn -> min)));
  }

  /**
   * Checks if the value extracted from the source is greater than another value provided by a function.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param target the function that provides the value to compare against
   * @return a predicate that evaluates if the extracted value is greater than the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThan(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(target)))
      .and(obj -> source.apply(obj).compareTo(target.apply(obj)) > ZERO);
  }
  
  /**
   * Checks if the value is greater than or equal to another value.
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param min the value to compare against
   * @return a predicate that evaluates if the input is greater than or equal to the provided value
   */
  public static <E, T extends Comparable<E>> Predicate<T> greaterThanOrEqual(final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(greaterThan(min).or(equalTo(min)));
  }

  /**
   * Checks if the value extracted from the source is greater than or equal to another value.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param min    the value to compare against
   * @return a predicate that evaluates if the extracted value is greater than or equal to the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThanOrEqual(final Function<T, E> source, final E min) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(greaterThanOrEqual(source, of(fn -> min)));
  }

  /**
   * Checks if the value extracted from the source is greater than or equal to another value provided by a function.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param target the function that provides the value to compare against
   * @return a predicate that evaluates if the extracted value is greater than or equal to the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> greaterThanOrEqual(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(target)))
      .and(greaterThan(source, target).or(equalTo(source, target)));
  }

  /**
   * Checks if the value is less than another value.
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param max the value to compare against
   * @return a predicate that evaluates if the input is less than the provided value
   */
  public static <E, T extends Comparable<E>> Predicate<T> lessThan(final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(fn -> max)))
      .and(lessThan -> lessThan.compareTo(max) < ZERO);
  }

  /**
   * Checks if the value extracted from the source is less than another value.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param max    the value to compare against
   * @return a predicate that evaluates if the extracted value is less than the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThan(final Function<T, E> source, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(lessThan(source, of(fn -> max)));
  }

  /**
   * Checks if the value extracted from the source is less than another value provided by a function.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param target the function that provides the value to compare against
   * @return a predicate that evaluates if the extracted value is less than the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThan(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(target)))
      .and(obj -> source.apply(obj).compareTo(target.apply(obj)) < ZERO);
  }

  /**
   * Checks if the value is less than or equal to another value.
   *
   * @param <E> the type of elements being compared
   * @param <T> the type of the object being evaluated, which must be comparable to E
   * @param max the value to compare against
   * @return a predicate that evaluates if the input is less than or equal to the provided value
   */
  public static <E, T extends Comparable<E>> Predicate<T> lessThanOrEqual(final E max) {
    return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).or(equalTo(max)));
  }

  /**
   * Checks if the value extracted from the source is less than or equal to another value.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param max    the value to compare against
   * @return a predicate that evaluates if the extracted value is less than or equal to the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThanOrEqual(final Function<T, E> source, final E max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(lessThanOrEqual(source, of((T fn) -> max)));
  }

  /**
   * Checks if the value extracted from the source is less than or equal to another value provided by a function.
   *
   * @param <T> the type of the object being evaluated
   * @param <E> the type of elements being compared
   * @param source the function to extract the value from the object
   * @param target the function that provides the value to compare against
   * @return a predicate that evaluates if the extracted value is less than or equal to the provided value
   */
  public static <T, E extends Comparable<E>> Predicate<T> lessThanOrEqual(final Function<T, E> source, final Function<T, E> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(not(nullValue(source)))
      .and(not(nullValue(target)))
      .and(obj -> lessThanOrEqual(target.apply(obj)).test(source.apply(obj)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private ComparablePredicate() {
    super();
  }

}