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

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.time.LocalTime;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Utility class providing predicates for LocalTime comparisons.
 * <p>
 * This class contains static factory methods that create {@link java.util.function.Predicate} instances
 * for various LocalTime comparison operations, including after, before, between, and equality checks.
 * All predicates support both direct LocalTime comparison and function-based extraction from objects.
 * </p>
 *
 * @see java.time.LocalTime
 * @see java.util.function.Predicate
 */
public final class LocalTimePredicate {

  /**
   * Creates a predicate that checks if a LocalTime is after the current time.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @return predicate that evaluates to true if input time is after the current time
   */
  public static <T extends LocalTime> Predicate<T> localTimeAfterNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTime -> localTimeAfter(LocalTime.now()).test(localTime));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is after the current time.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @return predicate that evaluates to true if source time is after the current time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeAfterNow(final Function<T, LocalTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeAfterNow().test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a LocalTime is before the current time.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @return predicate that evaluates to true if input time is before the current time
   */
  public static <T extends LocalTime> Predicate<T> localTimeBeforeNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTime -> localTimeBefore(LocalTime.now()).test(localTime));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is before the current time.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @return predicate that evaluates to true if source time is before the current time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeBeforeNow(final Function<T, LocalTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBeforeNow().test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a LocalTime is equal to a target LocalTime.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param localTime fixed LocalTime to compare against
   * @return predicate that evaluates to true if input time is equal to target time
   * @throws NullPointerException if localTime is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeEqualTo(final LocalTime localTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(localTime))
        .and(obj -> localTime.compareTo(obj) == 0);
  }

  /**
   * Creates a predicate that checks if a source LocalTime is equal to a target LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param localTime fixed LocalTime to compare against
   * @return predicate that evaluates to true if source time is equal to target time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeEqualTo(final Function<T, LocalTime> source, final LocalTime localTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeEqualTo(localTime).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a LocalTime is after a target LocalTime.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if input time is after target time
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeAfter(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isAfter(target));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is after a target LocalTime.
   * Source is a function that extracts LocalTime from the input object, target is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if source time is after target time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeAfter(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeAfter(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is after a target LocalTime.
   * Both source and target are functions that extract LocalTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target function to extract LocalTime from input object
   * @return predicate that evaluates to true if source time is after target time
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localTimeAfter(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeAfter(source, target.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a LocalTime is after or equal to a target LocalTime.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if input time is after or equal to target time
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeAfterOrEqual(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localTimeAfter(target)).or(localTimeEqualTo(target)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is after or equal to a target LocalTime.
   * Source is a function that extracts LocalTime from the input object, target is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if source time is after or equal to target time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeAfterOrEqual(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeAfterOrEqual(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is after or equal to a target LocalTime.
   * Both source and target are functions that extract LocalTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target function to extract LocalTime from input object
   * @return predicate that evaluates to true if source time is after or equal to target time
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localTimeAfterOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeAfterOrEqual(source, target.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a LocalTime is before a target LocalTime.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if input time is before target time
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeBefore(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isBefore(target));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is before a target LocalTime.
   * Source is a function that extracts LocalTime from the input object, target is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if source time is before target time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeBefore(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBefore(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is before a target LocalTime.
   * Both source and target are functions that extract LocalTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target function to extract LocalTime from input object
   * @return predicate that evaluates to true if source time is before target time
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localTimeBefore(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeBefore(source, target.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a LocalTime is before or equal to a target LocalTime.
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if input time is before or equal to target time
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeBeforeOrEqual(final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localTimeBefore(target)).or(localTimeEqualTo(target)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is before or equal to a target LocalTime.
   * Source is a function that extracts LocalTime from the input object, target is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target fixed LocalTime to compare against
   * @return predicate that evaluates to true if source time is before or equal to target time
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeBeforeOrEqual(final Function<T, LocalTime> source, final LocalTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBeforeOrEqual(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is before or equal to a target LocalTime.
   * Both source and target are functions that extract LocalTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param target function to extract LocalTime from input object
   * @return predicate that evaluates to true if source time is before or equal to target time
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localTimeBeforeOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localTimeBeforeOrEqual(source, target.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a LocalTime is between min and max times (exclusive).
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param min fixed minimum LocalTime to compare against
   * @param max fixed maximum LocalTime to compare against
   * @return predicate that evaluates to true if input time is between min and max times
   * @throws NullPointerException if min or max is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeBetween(final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTimeAfter(min).and(localTimeBefore(max)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (exclusive).
   * Source is a function that extracts LocalTime from the input object, min and max are fixed LocalTime values.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min fixed minimum LocalTime to compare against
   * @param max fixed maximum LocalTime to compare against
   * @return predicate that evaluates to true if source time is between min and max times
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBetween(min, max).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (exclusive).
   * Source and min are functions that extract LocalTime values from the input object, max is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min function to extract minimum LocalTime from input object
   * @param max fixed maximum LocalTime to compare against
   * @return predicate that evaluates to true if source time is between min and max times
   * @throws NullPointerException if source or min is null
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localTimeBetween(source, min.apply(obj), max).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (exclusive).
   * Source and max are functions that extract LocalTime values from the input object, min is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min fixed minimum LocalTime to compare against
   * @param max function to extract maximum LocalTime from input object
   * @return predicate that evaluates to true if source time is between min and max times
   * @throws NullPointerException if source or max is null
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final LocalTime min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetween(source, min, max.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (exclusive).
   * Source, min, and max are functions that extract LocalTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min function to extract minimum LocalTime from input object
   * @param max function to extract maximum LocalTime from input object
   * @return predicate that evaluates to true if source time is between min and max times
   * @throws NullPointerException if source, min, or max is null
   */
  public static <T> Predicate<T> localTimeBetween(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetween(source, min.apply(obj), max.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a LocalTime is between min and max times (inclusive).
   *
   * @param <T> the type of the input object (must be LocalTime or subclass)
   * @param min fixed minimum LocalTime to compare against
   * @param max fixed maximum LocalTime to compare against
   * @return predicate that evaluates to true if input time is between min and max times (inclusive)
   * @throws NullPointerException if min or max is null
   */
  public static <T extends LocalTime> Predicate<T> localTimeBetweenOrEqual(final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localTimeAfterOrEqual(min).and(localTimeBeforeOrEqual(max)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (inclusive).
   * Source is a function that extracts LocalTime from the input object, min and max are fixed LocalTime values.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min fixed minimum LocalTime to compare against
   * @param max fixed maximum LocalTime to compare against
   * @return predicate that evaluates to true if source time is between min and max times (inclusive)
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final LocalTime min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localTimeBetweenOrEqual(min, max).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (inclusive).
   * Source and min are functions that extract LocalTime values from the input object, max is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min function to extract minimum LocalTime from input object
   * @param max fixed maximum LocalTime to compare against
   * @return predicate that evaluates to true if source time is between min and max times (inclusive)
   * @throws NullPointerException if source or min is null
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final LocalTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localTimeBetweenOrEqual(source, min.apply(obj), max).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (inclusive).
   * Source and max are functions that extract LocalTime values from the input object, min is a fixed LocalTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min fixed minimum LocalTime to compare against
   * @param max function to extract maximum LocalTime from input object
   * @return predicate that evaluates to true if source time is between min and max times (inclusive)
   * @throws NullPointerException if source or max is null
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final LocalTime min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetweenOrEqual(source, min, max.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalTime is between min and max times (inclusive).
   * Source, min, and max are functions that extract LocalTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalTime from input object
   * @param min function to extract minimum LocalTime from input object
   * @param max function to extract maximum LocalTime from input object
   * @return predicate that evaluates to true if source time is between min and max times (inclusive)
   * @throws NullPointerException if source, min, or max is null
   */
  public static <T> Predicate<T> localTimeBetweenOrEqual(final Function<T, LocalTime> source, final Function<T, LocalTime> min, final Function<T, LocalTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localTimeBetweenOrEqual(source, min.apply(obj), max.apply(obj)).test(obj));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private LocalTimePredicate() {
    super();
  }

}
