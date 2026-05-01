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

import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Utility class that provides predicates for testing {@link Map} objects.
 */
public final class MapPredicate {

  /**
   * Creates a predicate that tests if the value for the specified key in a map matches the given predicate.
   *
   * @param <K> the type of keys in the map
   * @param <V> the type of values in the map
   * @param <T> the type of the input object (must be Map&lt;K, V&gt; or extends it)
   * @param key the key whose associated value is to be tested
   * @param predicate the predicate to apply to the value
   * @return a predicate that returns true if the map contains the key and its value matches the predicate
   * @throws NullPointerException if key or predicate is null
   */
  public static <K, V, T extends Map<K, V>> Predicate<T> mapGet(final K key, final Predicate<V> predicate) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> not(nullValue()).test(predicate))
      .and(obj -> predicate.test(obj.get(key)));
  }
  
  /**
   * Creates a predicate that tests if the value for a key extracted from the object matches the given predicate.
   *
   * @param <K> the type of keys in the map
   * @param <V> the type of values in the map
   * @param <T> the type of the input object (must be Map&lt;K, V&gt; or extends it)
   * @param key a function that extracts the key from the object
   * @param predicate the predicate to apply to the value
   * @return a predicate that returns true if the map contains the key and its value matches the predicate
   * @throws NullPointerException if key or predicate is null
   */
  public static <K, V, T extends Map<K, V>> Predicate<T> mapGet(final Function<T, K> key, final Predicate<V> predicate) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> not(nullValue()).test(key.apply(obj)))
      .and(obj -> not(nullValue()).test(predicate))
      .and(obj -> predicate.test(obj.get(key.apply(obj))));
  }
  
  /**
   * Creates a predicate that tests if a map contains the specified key.
   *
   * @param <K> the type of keys in the map
   * @param <V> the type of values in the map
   * @param <T> the type of the input object (must be Map&lt;K, V&gt; or extends it)
   * @param key the key to check for
   * @return a predicate that returns true if the map contains the specified key
   * @throws NullPointerException if key is null
   */
  public static <K, V, T extends Map<K, V>> Predicate<T> containsKey(final K key) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> obj.containsKey(key));
  }

  /**
   * Creates a predicate that tests if a map contains a key extracted from the object.
   *
   * @param <K> the type of keys in the map
   * @param <V> the type of values in the map
   * @param <T> the type of the input object (must be Map&lt;K, V&gt; or extends it)
   * @param key a function that extracts the key from the object
   * @return a predicate that returns true if the map contains the extracted key
   * @throws NullPointerException if key is null
   */
  public static <K, V, T extends Map<K, V>> Predicate<T> containsKey(final Function<T, K> key) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> not(nullValue()).test(key.apply(obj)))
      .and(obj -> obj.containsKey(key.apply(obj)));
  }
  
  /**
   * Creates a predicate that tests if a map contains the specified value.
   *
   * @param <K> the type of keys in the map
   * @param <V> the type of values in the map
   * @param <T> the type of the input object (must be Map&lt;K, V&gt; or extends it)
   * @param value the value to check for
   * @return a predicate that returns true if the map contains the specified value
   */
  public static <K, V, T extends Map<K, V>> Predicate<T> containsValue(final V value) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> obj.containsValue(value));
  }
  
  /**
   * Creates a predicate that tests if a map contains a value extracted from the object.
   *
   * @param <K> the type of keys in the map
   * @param <V> the type of values in the map
   * @param <T> the type of the input object (must be Map&lt;K, V&gt; or extends it)
   * @param value a function that extracts the value from the object
   * @return a predicate that returns true if the map contains the extracted value
   * @throws NullPointerException if value is null
   */
  public static <K, V, T extends Map<K, V>> Predicate<T> containsValue(final Function<T, V> value) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(value))
      .and(obj -> obj.containsValue(value.apply(obj)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private MapPredicate() {
    super();
  }

}
