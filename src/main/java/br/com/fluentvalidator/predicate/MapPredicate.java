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

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

public final class MapPredicate {

  private MapPredicate() {
    super();
  }
  
  public static <K, V, T extends Map<K, V>> Predicate<T> mapGet(final K key, final Predicate<V> predicate) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> not(nullValue()).test(predicate))
      .and(obj -> predicate.test(obj.get(key)));
  }
  
  public static <K, V, T extends Map<K, V>> Predicate<T> mapGet(final Function<T, K> key, final Predicate<V> predicate) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> not(nullValue()).test(key.apply(obj)))
      .and(obj -> not(nullValue()).test(predicate))
      .and(obj -> predicate.test(obj.get(key.apply(obj))));
  }
  
  public static <K, V, T extends Map<K, V>> Predicate<T> containsKey(final K key) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> obj.containsKey(key));
  }

  public static <K, V, T extends Map<K, V>> Predicate<T> containsKey(final Function<T, K> key) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(key))
      .and(obj -> not(nullValue()).test(key.apply(obj)))
      .and(obj -> obj.containsKey(key.apply(obj)));
  }
  
  public static <K, V, T extends Map<K, V>> Predicate<T> containsValue(final V value) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> obj.containsValue(value));
  }
  
  public static <K, V, T extends Map<K, V>> Predicate<T> containsValue(final Function<T, V> value) {
    return PredicateBuilder.<T>from(not(nullValue()))
      .and(obj -> not(nullValue()).test(value))
      .and(obj -> obj.containsValue(value.apply(obj)));
  }

}
