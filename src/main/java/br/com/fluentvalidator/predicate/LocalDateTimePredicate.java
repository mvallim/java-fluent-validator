/*
 * Copyright 2023 the original author or authors.
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

public final class LocalDateTimePredicate {

  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateAfterToday().test(localDateTime.toLocalDate()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfterToday(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterOrEqualToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateAfterOrEqualToday().test(localDateTime.toLocalDate()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfterOrEqualToday(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterOrEqualToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateBeforeToday().test(localDateTime.toLocalDate()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBeforeToday(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeOrEqualToday() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateBeforeOrEqualToday().test(localDateTime.toLocalDate()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBeforeOrEqualToday(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeOrEqualToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeIsToday(){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateIsToday().test(localDateTime.toLocalDate()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeIsToday(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeIsToday().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateTimeAfter(LocalDateTime.now()).test(localDateTime));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfterNow(final Function<T, LocalDateTime> source){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterNow().test(source.apply(obj)));
  }


  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateTimeBefore(LocalDateTime.now()).test(localDateTime));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBeforeNow(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeNow().test(source.apply(obj)));
  }


  /**
   *
   * @param localDateTime
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeEqualTo(final LocalDateTime localDateTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(localDateTime))
        .and(obj -> localDateTime.isEqual(obj));
  }

  /**
   *
   * @param source
   * @param localDateTime
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeEqualTo(final Function<T, LocalDateTime> source, final LocalDateTime localDateTime){
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeEqualTo(localDateTime).test(source.apply(obj)));
  }


  /**
   *
   * @param target
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfter(final LocalDateTime target) {
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
  public static <T> Predicate<T> localDateTimeAfter(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfter(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfter(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeAfter(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param target
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterOrEqual(final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localDateTimeAfter(target)).or(localDateTimeEqualTo(target)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfterOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeAfterOrEqual(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfterOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeAfterOrEqual(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param target
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBefore(final LocalDateTime target) {
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
  public static <T> Predicate<T> localDateTimeBefore(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBefore(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBefore(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeBefore(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param target
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeOrEqual(final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target))
        .and(is(localDateTimeBefore(target)).or(localDateTimeEqualTo(target)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBeforeOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBeforeOrEqual(target).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBeforeOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> localDateTimeBeforeOrEqual(source, target.apply(obj)).test(obj));
  }


  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBetween(final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTimeAfter(min).and(localDateTimeBefore(max)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBetween(min, max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localDateTimeBetween(source, min.apply(obj), max).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final LocalDateTime min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetween(source, min, max.apply(obj)).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetween(source, min.apply(obj), max.apply(obj)).test(obj));
  }


  /**
   *
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBetweenOrEqual(final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTimeAfterOrEqual(min).and(localDateTimeBeforeOrEqual(max)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> localDateTimeBetweenOrEqual(min, max).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(obj -> localDateTimeBetweenOrEqual(source, min.apply(obj), max).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final LocalDateTime min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetweenOrEqual(source, min, max.apply(obj)).test(obj));
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetweenOrEqual(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> min, final Function<T, LocalDateTime> max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(min)))
        .and(not(nullValue(max)))
        .and(obj -> localDateTimeBetweenOrEqual(source, min.apply(obj), max.apply(obj)).test(obj));
  }


  private LocalDateTimePredicate() {
    super();
  }

}
