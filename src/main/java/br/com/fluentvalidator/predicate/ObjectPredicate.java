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

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Provides predicate factory methods for common object operations such as equality checks,
 * instance of checks, and null value checks.
 *
 * <p>This class contains static methods that return {@link Predicate} instances for
 * validating object properties and states.</p>
 */
public final class ObjectPredicate {

  /**
   * Creates a predicate that checks if objects extracted from the input using the provided
   * source and target functions are equal.
   *
   * @param <T> the type of the input object
   * @param source function to extract value from input object
   * @param target function to extract target value to compare against
   * @return predicate that evaluates to true if the extracted objects are equal
   */
  public static <T> Predicate<T> equalObject(final Function<T, Object> source, final Function<T, Object> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> Objects.equals(source.apply(obj), target.apply(obj)));
  }

  /**
   * Creates a predicate that checks if the object extracted from the input using the provided
   * source function equals the fixed target value.
   *
   * @param <T> the type of the input object
   * @param source function to extract value from input object
   * @param target fixed value to compare against
   * @return predicate that evaluates to true if the extracted object equals the target
   */
  public static <T> Predicate<T> equalObject(final Function<T, Object> source, final Object target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(obj -> target)))
        .and(obj -> Objects.equals(source.apply(obj), target));
  }

  /**
   * Creates a predicate that checks if the input object equals the fixed target value.
   *
   * @param <T> the type of the input object
   * @param target fixed value to compare against
   * @return predicate that evaluates to true if the input object equals the target
   */
  public static <T> Predicate<T> equalObject(final Object target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> Objects.equals(obj, target));
  }

  /**
   * Creates a predicate that checks if the input object is an instance of the specified class.
   *
   * @param <T> the type of the input object
   * @param clazz the class to check instance against
   * @return predicate that evaluates to true if the input object is an instance of the specified class
   */
  @SuppressWarnings("java:S1612")
  public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(fn -> clazz)))
        .and(obj -> clazz.isInstance(obj));
  }

  /**
   * Creates a predicate that checks if the object extracted from the input using the provided
   * source function is an instance of the specified class.
   *
   * @param <T> the type of the input object
   * @param source function to extract value from input object
   * @param clazz the class to check instance against
   * @return predicate that evaluates to true if the extracted object is an instance of the specified class
   */
  public static <T> Predicate<T> instanceOf(final Function<T, ?> source, final Class<?> clazz) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> instanceOf(clazz).test(source.apply(obj)));
  }

  /**
   * Creates a predicate that checks if the input object is null.
   *
   * @param <T> the type of the input object
   * @return predicate that evaluates to true if the input object is null
   */
  public static <T> Predicate<T> nullValue() {
    return PredicateBuilder.<T>from(Objects::isNull);
  }

  /**
   * Creates a predicate that checks if the object extracted from the input using the provided
   * source function is null.
   *
   * @param <T> the type of the input object
   * @param source function to extract value from input object
   * @return predicate that evaluates to true if the extracted object is null
   */
  public static <T> Predicate<T> nullValue(final Function<T, ?> source) {
    return PredicateBuilder.<T>from(nullValue())
        .or(obj -> Objects.isNull(source))
        .or(obj -> Objects.isNull(source.apply(obj)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private ObjectPredicate() {
    super();
  }

}
