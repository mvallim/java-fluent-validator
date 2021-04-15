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
