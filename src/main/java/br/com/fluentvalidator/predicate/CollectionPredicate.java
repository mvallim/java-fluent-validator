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

/**
 * Utility class providing predicate methods for validating collections.
 * This class contains static methods that return predicates for common collection validation scenarios
 * such as checking if a collection is empty, contains specific items, or has a certain size.
 */
public final class CollectionPredicate {

  /**
   * Creates a predicate that tests if a collection is empty (null or has no elements).
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @return a predicate that returns true if the collection is null or empty
   */
  public static <E, T extends Collection<E>> Predicate<T> empty() {
    return PredicateBuilder.<T>from(is(nullValue()))
      .or(Collection::isEmpty);
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object is empty.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @return a predicate that returns true if the extracted collection is null or empty
   */
  public static <T, E> Predicate<T> empty(final Function<T, Collection<E>> source) {
    return PredicateBuilder.<T>from(is(nullValue()))
      .or(obj -> is(nullValue()).test(source.apply(obj)))
      .or(obj -> source.apply(obj).isEmpty());
  }

  /**
   * Creates a predicate that tests if a collection contains any of the specified objects.
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param objects the collection of objects to check for
   * @return a predicate that returns true if the collection contains any of the specified objects
   */
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(hasAny -> hasAny.stream().anyMatch(objects::contains));
  }

  /**
   * Creates a predicate that tests if a collection contains any of the specified objects from an array.
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param objects the array of objects to check for
   * @return a predicate that returns true if the collection contains any of the specified objects
   */
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(obj -> hasAny(Arrays.asList(objects)).test(obj));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object contains any of the specified objects.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param objects the collection of objects to check for
   * @return a predicate that returns true if the extracted collection contains any of the specified objects
   */
  public static <T, E> Predicate<T> hasAny(final Function<T, Collection<E>> source, final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasAny(objects).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object contains any of the specified objects from an array.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param objects the array of objects to check for
   * @return a predicate that returns true if the extracted collection contains any of the specified objects
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
   * @return {@link Predicate} that returns true if the collection contains the specified object
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> obj.contains(object));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object contains the specified item.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param object the object to check for in the collection
   * @return a predicate that returns true if the extracted collection contains the specified object
   */
  public static <T, E> Predicate<T> hasItem(final Function<T, Collection<E>> source, final E object) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasItem(object).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a collection contains all of the specified objects.
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param objects the collection of objects that must all be present
   * @return a predicate that returns true if the collection contains all specified objects
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(obj -> obj.containsAll(objects));
  }

  /**
   * Creates a predicate that tests if a collection contains all of the specified objects from an array.
   * 
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param objects the array of objects that must all be present
   * @return a predicate that returns true if the collection contains all specified objects
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItems(final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(objects))
      .and(obj -> hasItems(Arrays.asList(objects)).test(obj));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object contains all of the specified objects.
   * 
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param objects the collection of objects that must all be present
   * @return a predicate that returns true if the extracted collection contains all specified objects
   */
  public static <T, E> Predicate<T> hasItems(final Function<T, Collection<E>> source, final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasItems(objects).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object contains all of the specified objects from an array.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param objects the array of objects that must all be present
   * @return a predicate that returns true if the extracted collection contains all specified objects
   */
  public static <T, E> Predicate<T> hasItems(final Function<T, Collection<E>> source, final E[] objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> hasItems(objects).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object has a size equal to a dynamically computed value.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param size a function that computes the expected size from the object
   * @return a predicate that returns true if the extracted collection has the computed size
   */
  public static <T, E> Predicate<T> hasSize(final Function<T, Collection<E>> source, final Function<T, Integer> size) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(size)).test(obj))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> equalTo(size.apply(obj)).test(source.apply(obj).size()));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object has a specific size.
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param size the expected size of the collection
   * @return a predicate that returns true if the extracted collection has the specified size
   */
  public static <T, E> Predicate<T> hasSize(final Function<T, Collection<E>> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> equalTo(size).test(source.apply(obj).size()));
  }

  /**
   * Creates a predicate that tests if a collection has a specific size.
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param size the expected size of the collection
   * @return a predicate that returns true if the collection has the specified size
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSize(final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> equalTo(size).test(obj.size()));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object has a size between the specified bounds (exclusive).
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param min the minimum size (exclusive)
   * @param max the maximum size (exclusive)
   * @return a predicate that returns true if the extracted collection size is between min and max (exclusive)
   */
  public static <T, E> Predicate<T> hasSizeBetween(final Function<T, Collection<E>> source, final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> between(min, max).test(source.apply(obj).size()));
  }

  /**
   * Creates a predicate that tests if a collection has a size between the specified bounds (exclusive).
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param min the minimum size (exclusive)
   * @param max the maximum size (exclusive)
   * @return a predicate that returns true if the collection size is between min and max (exclusive)
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSizeBetween(final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(between(Collection::size, min, max));
  }

  /**
   * Creates a predicate that tests if a collection extracted from an object has a size between the specified bounds (inclusive).
   *
   * @param <T> the type of the object being tested
   * @param <E> the type of elements in the collection
   * @param source a function that extracts a collection from the object
   * @param min the minimum size (inclusive)
   * @param max the maximum size (inclusive)
   * @return a predicate that returns true if the extracted collection size is between min and max (inclusive)
   */
  public static <T, E> Predicate<T> hasSizeBetweenInclusive(final Function<T, Collection<E>> source, final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue(source)).test(obj))
      .and(obj -> betweenInclusive(min, max).test(source.apply(obj).size()));
  }

  /**
   * Creates a predicate that tests if a collection has a size between the specified bounds (inclusive).
   *
   * @param <E> the type of elements in the collection
   * @param <T> the type of collection that extends Collection&lt;E&gt;
   * @param min the minimum size (inclusive)
   * @param max the maximum size (inclusive)
   * @return a predicate that returns true if the collection size is between min and max (inclusive)
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSizeBetweenInclusive(final Integer min, final Integer max) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(betweenInclusive(Collection::size, min, max));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private CollectionPredicate() {
    super();
  }

}
