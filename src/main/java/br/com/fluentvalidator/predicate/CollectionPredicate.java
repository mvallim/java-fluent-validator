package br.com.fluentvalidator.predicate;

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
    return PredicateBuilder.<T>from(is(nullValue())).or(Collection::isEmpty);
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
        .or(obj -> is(nullValue()).test(source.apply(obj))).or(obj -> source.apply(obj).isEmpty());
  }

  /**
   *
   * @param <E>
   * @param <T>
   * @param objects
   * @return
   */
  public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects))
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
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects))
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
   * Creates a predicate for {@link Collection} that only matches when a single pass over the
   * examined {@link Collection} yields at least one item that is equal to the specified
   * <code>object</code>. Whilst matching, the traversal of the examined {@link Collection} will
   * stop as soon as a matching item is found.
   *
   * <pre>
   * ruleForEach(Parent::getNames).must(hasItem("John"));
   * </pre>
   *
   * @param <E> type of object
   * @param <T> type of exam class
   * @param object the object to compare against the objects provided by the examined
   *        {@link Collection}
   * @return {@link Predicate}
   */
  public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> obj.contains(object));
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
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects))
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
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects))
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
  public static <T, E> Predicate<T> hasSize(final Function<T, Collection<E>> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
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
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> equalTo(size).test(obj.size()));
  }

  private CollectionPredicate() {
    super();
  }

}
