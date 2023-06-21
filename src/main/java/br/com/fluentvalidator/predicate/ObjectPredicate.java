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

public final class ObjectPredicate {

  /**
   * 
   * @param <T>
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> equalObject(final Function<T, Object> source, final Function<T, Object> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> Objects.equals(source.apply(obj), target.apply(obj)));
  }

  /**
   * 
   * @param <T>
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> equalObject(final Function<T, Object> source, final Object target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(obj -> target)))
        .and(obj -> Objects.equals(source.apply(obj), target));
  }

  /**
   * 
   * @param <T>
   * @param obj
   * @return
   */
  public static <T> Predicate<T> equalObject(final Object target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> Objects.equals(obj, target));
  }

  /**
   * 
   * @param <T>
   * @param clazz
   * @return
   */
  @SuppressWarnings("java:S1612")
  public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(fn -> clazz)))
        .and(obj -> clazz.isInstance(obj));
  }

  /**
   *
   * @param <T>
   * @param source
   * @param clazz
   * @return
   */
  public static <T> Predicate<T> instanceOf(final Function<T, ?> source, final Class<?> clazz) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> instanceOf(clazz).test(source.apply(obj)));
  }

  /**
   * 
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> nullValue() {
    return PredicateBuilder.<T>from(Objects::isNull);
  }

  /**
   * 
   * @param <T>
   * @param source
   * @return
   */
  public static <T> Predicate<T> nullValue(final Function<T, ?> source) {
    return PredicateBuilder.<T>from(nullValue())
        .or(obj -> Objects.isNull(source))
        .or(obj -> Objects.isNull(source.apply(obj)));
  }
  
  private ObjectPredicate() {
    super();
  }

}
