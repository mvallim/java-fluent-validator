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

import static br.com.fluentvalidator.predicate.ComparablePredicate.between;
import static br.com.fluentvalidator.predicate.ComparablePredicate.betweenInclusive;
import static br.com.fluentvalidator.predicate.ComparablePredicate.equalTo;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

public final class CollectionPredicate {

  /**
   *
   * @param <E>
   * @param <T>
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> empty() {
    return PredicateBuilder.<T>from(is(nullValue()))
      .or(Collection::isEmpty);
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @return
   */
  public static <T, E> Predicate<T> empty(final Function<T, Collection<E>> source) {
    return PredicateBuilder.<T>from(is(nullValue()))
      .or(obj -> is(nullValue()).test(source.apply(obj)))
      .or(obj -> source.apply(obj).isEmpty());
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(hasAny -> hasAny.stream().anyMatch(objects::contains));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(obj -> hasAny(Arrays.asList(objects)).test(obj));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param objects
   * @return
   */
  public static <T, E> Predicate<T> hasAny(final Function<T, Collection<E>> source, final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasAny(objects).test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param objects
   * @return
   */
  public static <T, E> Predicate<T> hasAny(final Function<T, Collection<E>> source, final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasAny(objects).test(source.apply(obj)));
  }

  /**
   * Creates a predicate for {@link Collection} that only matches when a single
   * pass over the examined {@link Collection} yields at least one item that is
   * equal to the specified <code>object</code>. Whilst matching, the traversal of
   * the examined {@link Collection} will stop as soon as a matching item is
   * found.
   *
   * <pre>
   * ruleForEach(Parent::getNames).must(hasItem("John"));
   * </pre>
   *
   * @param <E>    type of object
   * @param <T>    type of exam class
   * @param object the object to compare against the objects provided by the
   *               examined {@link Collection}
   * @return {@link Predicate}
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> obj.contains(object));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param object
   * @return
   */
  public static <T, E> Predicate<T> hasItem(final Function<T, Collection<E>> source, final E object) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasItem(object).test(source.apply(obj)));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(obj -> obj.containsAll(objects));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItems(final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(obj -> hasItems(Arrays.asList(objects)).test(obj));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param objects
   * @return
   */
  public static <T, E> Predicate<T> hasItems(final Function<T, Collection<E>> source, final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasItems(objects).test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param objects
   * @return
   */
  public static <T, E> Predicate<T> hasItems(final Function<T, Collection<E>> source, final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasItems(objects).test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param size
   * @return
   */
  public static <T, E> Predicate<T> hasSize(final Function<T, Collection<E>> source, final Function<T, Integer> size) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(size)).test(obj))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> equalTo(size.apply(obj)).test(source.apply(obj).size()));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param size
   * @return
   */
  public static <T, E> Predicate<T> hasSize(final Function<T, Collection<E>> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> equalTo(size).test(source.apply(obj).size()));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param size
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSize(final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> equalTo(size).test(obj.size()));
  }

  /**
   * 
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E> Predicate<T> hasSizeBetween(final Function<T, Collection<E>> source, final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> between(min, max).test(source.apply(obj).size()));
  }

  /**
   * 
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSizeBetween(final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(between(Collection::size, min, max));
  }

  /**
   *
   * @param <T>
   * @param <E>
   * @param source
   * @param min
   * @param max
   * @return
   */
  public static <T, E> Predicate<T> hasSizeBetweenInclusive(final Function<T, Collection<E>> source, final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> betweenInclusive(min, max).test(source.apply(obj).size()));
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param min
   * @param max
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSizeBetweenInclusive(final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(Collection::size, min, max));
  }

  private CollectionPredicate() {
    super();
  }

}
