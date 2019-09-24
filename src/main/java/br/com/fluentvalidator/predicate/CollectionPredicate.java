package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ComparablePredicate.equalTo;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

public final class CollectionPredicate {

	private CollectionPredicate() {
		super();
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @return
	 */
	public static <E, T extends Collection<E>> Predicate<T> empty() {
		return PredicateBuilder.<T>from(not(nullValue())).and(Collection::isEmpty);
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param object
	 * @return
	 */
	public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> obj.contains(object));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param objects
	 * @return
	 */
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects)).and(obj -> obj.containsAll(objects));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param objects
	 * @return
	 */
	@SafeVarargs
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final E... objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects)).and(obj -> hasItems(Arrays.asList(objects)).test(obj));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param objects
	 * @return
	 */
	public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects)).and(hasAny -> hasAny.stream().anyMatch(item -> objects.contains(item)));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param objects
	 * @return
	 */
	@SafeVarargs
	public static <E, T extends Collection<E>> Predicate<T> hasAny(final E... objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(objects)).and(obj -> hasAny(Arrays.asList(objects)).test(obj));
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

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @return
	 */
	public static <T, E extends Collection<E>> Predicate<T> empty(final Function<T, E> source) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(source.apply(obj))).and(obj -> source.apply(obj).isEmpty());
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param object
	 * @return
	 */
	public static <T, E extends Collection<E>> Predicate<T> hasItem(final Function<T, E> source, final E object) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> hasItem(object).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param objects
	 * @return
	 */
	public static <T, E extends Collection<E>> Predicate<T> hasItems(final Function<T, E> source, final Collection<E> objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> hasItems(objects).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param objects
	 * @return
	 */
	@SafeVarargs
	public static <T, E extends Collection<E>> Predicate<T> hasItems(final Function<T, E> source, final E... objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> hasItems(objects).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param objects
	 * @return
	 */
	public static <T, E extends Collection<E>> Predicate<T> hasAny(final Function<T, E> source, final Collection<E> objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> hasAny(objects).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param objects
	 * @return
	 */
	@SafeVarargs
	public static <T, E extends Collection<E>> Predicate<T> hasAny(final Function<T, E> source, final E... objects) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> hasAny(objects).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param size
	 * @return
	 */
	public static <T, E extends Collection<E>> Predicate<T> hasSize(final Function<T, E> source, final Integer size) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> equalTo(size).test(source.apply(obj).size()));
	}

}
