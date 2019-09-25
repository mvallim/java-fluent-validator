package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Function;
import java.util.function.Predicate;

public final class ComparablePredicate {

	private static final Integer ZERO = 0;

	private ComparablePredicate() {
		super();
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
	public static <T, E extends Comparable<E>> Predicate<T> between(final Function<T, E> source, final E min, final E max) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> between(min, max).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param value
	 * @return
	 */
	public static <T, E extends Comparable<E>> Predicate<T> equalTo(final Function<T, E> source, final E value) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> equalTo(value).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param max
	 * @return
	 */
	public static <T, E extends Comparable<E>> Predicate<T> lessThan(final Function<T, E> source, final E max) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> lessThan(max).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param max
	 * @return
	 */
	public static <T, E extends Comparable<E>> Predicate<T> lessThanOrEqual(final Function<T, E> source, final E max) {
		return PredicateBuilder.<T>from(lessThan(source, max).or(equalTo(source, max)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param min
	 * @return
	 */
	public static <T, E extends Comparable<E>> Predicate<T> greaterThan(final Function<T, E> source, final E min) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> greaterThan(min).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param <E>
	 * @param source
	 * @param min
	 * @return
	 */
	public static <T, E extends Comparable<E>> Predicate<T> greaterThanOrEqual(final Function<T, E> source, final E min) {
		return PredicateBuilder.<T>from(greaterThan(source, min).or(equalTo(source, min)));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param value
	 * @return
	 */
	public static <E, T extends Comparable<E>> Predicate<T> equalTo(final E value) {
		return PredicateBuilder.<T>from(not(nullValue())).and(equalTo -> not(nullValue()).test(value)).and(equalTo -> equalTo.compareTo(value) == ZERO);
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param max
	 * @return
	 */
	public static <E, T extends Comparable<E>> Predicate<T> lessThan(final E max) {
		return PredicateBuilder.<T>from(not(nullValue())).and(lessThan -> not(nullValue()).test(max)).and(lessThan -> lessThan.compareTo(max) < ZERO);
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param min
	 * @return
	 */
	public static <E, T extends Comparable<E>> Predicate<T> greaterThan(final E min) {
		return PredicateBuilder.<T>from(not(nullValue())).and(greaterThan -> not(nullValue()).test(min)).and(greaterThan -> greaterThan.compareTo(min) > ZERO);
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param min
	 * @return
	 */
	public static <E, T extends Comparable<E>> Predicate<T> greaterThanOrEqual(final E min) {
		return PredicateBuilder.<T>from(not(nullValue())).and(greaterThan(min).or(equalTo(min)));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param max
	 * @return
	 */
	public static <E, T extends Comparable<E>> Predicate<T> lessThanOrEqual(final E max) {
		return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).or(equalTo(max)));
	}

	/**
	 *
	 * @param <E>
	 * @param <T>
	 * @param min
	 * @param max
	 * @return
	 */
	public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final E max) {
		return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).and(greaterThan(min)));
	}

}
