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

import java.time.LocalDateTime;
import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.predicate.LocalDatePredicate.*;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

/**
 * Utility class providing predicates for LocalDateTime comparisons.
 * <p>
 * This class contains static factory methods that create {@link java.util.function.Predicate} instances
 * for various LocalDateTime comparison operations, including after, before, between, equality checks,
 * and date-based comparisons (today, now).
 * </p>
 *
 * @see java.time.LocalDateTime
 * @see java.util.function.Predicate
 */
public final class LocalDateTimePredicate {

  /**
   * Creates a predicate that checks if a LocalDateTime is after today's date.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is after today's date
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateAfterToday().test(localDateTime.toLocalDate()));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after today's date.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is after today's date
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeAfterToday(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterToday().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is after or equal to today's date.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is after or equal to today's date
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterOrEqualToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateAfterOrEqualToday().test(localDateTime.toLocalDate()));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after or equal to today's date.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is after or equal to today's date
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeAfterOrEqualToday(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterOrEqualToday().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is before today's date.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is before today's date
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateBeforeToday().test(localDateTime.toLocalDate()));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before today's date.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is before today's date
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBeforeToday(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeToday().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is before or equal to today's date.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is before or equal to today's date
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeOrEqualToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateBeforeOrEqualToday().test(localDateTime.toLocalDate()));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before or equal to today's date.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is before or equal to today's date
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBeforeOrEqualToday(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeOrEqualToday().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is equal to today's date.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is equal to today's date
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeIsToday(){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateIsToday().test(localDateTime.toLocalDate()));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is equal to today's date.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is equal to today's date
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeIsToday(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeIsToday().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is after the current date and time.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is after now
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateTimeAfter(LocalDateTime.now()).test(localDateTime));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after the current date and time.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is after now
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeAfterNow(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterNow().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is before the current date and time.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @return predicate that evaluates to true if input datetime is before now
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateTimeBefore(LocalDateTime.now()).test(localDateTime));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before the current date and time.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is before now
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBeforeNow(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeNow().test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is equal to a target LocalDateTime.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param localDateTime fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is equal to target datetime
   * @throws NullPointerException if localDateTime is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeEqualTo(final LocalDateTime localDateTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(localDateTime))
        .and(obj -> localDateTime.isEqual(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is equal to a target LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param localDateTime fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is equal to target datetime
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeEqualTo(final Function<T, LocalDateTime> source, final LocalDateTime localDateTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeEqualTo(localDateTime).test(source.apply(obj)));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is after a target LocalDateTime.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is after target datetime
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfter(final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isAfter(target));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after a target LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is after target datetime
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeAfter(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfter(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after a target LocalDateTime.
   * Both source and target are functions that extract LocalDateTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is after target datetime
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localDateTimeAfter(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeAfter(source, target.apply(obj)).test(obj));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is after or equal to a target LocalDateTime.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is after or equal to target datetime
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterOrEqual(final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localDateTimeAfter(target)).or(localDateTimeEqualTo(target)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after or equal to a target LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is after or equal to target datetime
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeAfterOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterOrEqual(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is after or equal to a target LocalDateTime.
   * Both source and target are functions that extract LocalDateTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is after or equal to target datetime
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localDateTimeAfterOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeAfterOrEqual(source, target.apply(obj)).test(obj));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is before a target LocalDateTime.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is before target datetime
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBefore(final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(obj -> obj.isBefore(target));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before a target LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is before target datetime
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBefore(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBefore(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before a target LocalDateTime.
   * Both source and target are functions that extract LocalDateTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is before target datetime
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localDateTimeBefore(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeBefore(source, target.apply(obj)).test(obj));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is before or equal to a target LocalDateTime.
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is before or equal to target datetime
   * @throws NullPointerException if target is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeOrEqual(final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localDateTimeBefore(target)).or(localDateTimeEqualTo(target)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before or equal to a target LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target fixed LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is before or equal to target datetime
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBeforeOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeOrEqual(target).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is before or equal to a target LocalDateTime.
   * Both source and target are functions that extract LocalDateTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param target function to extract LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is before or equal to target datetime
   * @throws NullPointerException if source or target is null
   */
  public static <T> Predicate<T> localDateTimeBeforeOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeBeforeOrEqual(source, target.apply(obj)).test(obj));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is between min and max datetimes (exclusive).
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param min fixed minimum LocalDateTime to compare against
   * @param max fixed maximum LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is between min and max datetimes
   * @throws NullPointerException if min or max is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBetween(final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTimeAfter(min).and(localDateTimeBefore(max)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (exclusive).
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min fixed minimum LocalDateTime to compare against
   * @param max fixed maximum LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is between min and max datetimes
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBetween(min, max).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (exclusive).
   * Source and min are functions, max is a fixed LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min function to extract minimum LocalDateTime from input object
   * @param max fixed maximum LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is between min and max datetimes
   * @throws NullPointerException if source or min is null
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localDateTimeBetween(source, min.apply(obj), max).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (exclusive).
   * Source and max are functions, min is a fixed LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min fixed minimum LocalDateTime to compare against
   * @param max function to extract maximum LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is between min and max datetimes
   * @throws NullPointerException if source or max is null
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final LocalDateTime min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetween(source, min, max.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (exclusive).
   * Source, min, and max are all functions that extract LocalDateTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min function to extract minimum LocalDateTime from input object
   * @param max function to extract maximum LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is between min and max datetimes
   * @throws NullPointerException if source, min, or max is null
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetween(source, min.apply(obj), max.apply(obj)).test(obj));
  }


  /**
   * Creates a predicate that checks if a LocalDateTime is between min and max datetimes (inclusive).
   *
   * @param <T> the type of the input object (must be LocalDateTime or subclass)
   * @param min fixed minimum LocalDateTime to compare against
   * @param max fixed maximum LocalDateTime to compare against
   * @return predicate that evaluates to true if input datetime is between min and max datetimes (inclusive)
   * @throws NullPointerException if min or max is null
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBetweenOrEqual(final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTimeAfterOrEqual(min).and(localDateTimeBeforeOrEqual(max)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (inclusive).
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min fixed minimum LocalDateTime to compare against
   * @param max fixed maximum LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is between min and max datetimes (inclusive)
   * @throws NullPointerException if source is null
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBetweenOrEqual(min, max).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (inclusive).
   * Source and min are functions, max is a fixed LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min function to extract minimum LocalDateTime from input object
   * @param max fixed maximum LocalDateTime to compare against
   * @return predicate that evaluates to true if source datetime is between min and max datetimes (inclusive)
   * @throws NullPointerException if source or min is null
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localDateTimeBetweenOrEqual(source, min.apply(obj), max).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (inclusive).
   * Source and max are functions, min is a fixed LocalDateTime.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min fixed minimum LocalDateTime to compare against
   * @param max function to extract maximum LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is between min and max datetimes (inclusive)
   * @throws NullPointerException if source or max is null
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetweenOrEqual(source, min, max.apply(obj)).test(obj));
  }

  /**
   * Creates a predicate that checks if a source LocalDateTime is between min and max datetimes (inclusive).
   * Source, min, and max are all functions that extract LocalDateTime values from the input object.
   *
   * @param <T> the type of the input object
   * @param source function to extract LocalDateTime from input object
   * @param min function to extract minimum LocalDateTime from input object
   * @param max function to extract maximum LocalDateTime from input object
   * @return predicate that evaluates to true if source datetime is between min and max datetimes (inclusive)
   * @throws NullPointerException if source, min, or max is null
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetweenOrEqual(source, min.apply(obj), max.apply(obj)).test(obj));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private LocalDateTimePredicate() {
    super();
  }

}
