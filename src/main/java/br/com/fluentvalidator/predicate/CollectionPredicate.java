package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Predicate;

public final class CollectionPredicate {

  private CollectionPredicate() {
    super();
  }

  /**
   *
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> empty() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(Collection::isEmpty);
  }

  /**
   *
   * @param object
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(hasItem -> hasItem.contains(object));
  }

  /**
   *
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(hasItems -> hasItems.containsAll(objects));
  }

  /**
   *
   * @param objects
   * @return
   */
  @SafeVarargs
  public static <E, T extends Collection<E>> Predicate<T> hasItems(final E... objects) {
    return hasItems(Arrays.asList(objects));
  }

  /**
   *
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue())).and(hasItems -> {
      for (final Object object : objects) {
        if (hasItems.contains(object)) {
          return true;
        }
      }
      return false;
    });
  }

  /**
   *
   * @param objects
   * @return
   */
  @SafeVarargs
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final E... objects) {
    return hasAny(Arrays.asList(objects));
  }

  /**
   *
   * @param size
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasSize(final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(hasSize -> not(nullValue()).test(size))
        .and(hasSize -> hasSize.size() == size);
  }

}
