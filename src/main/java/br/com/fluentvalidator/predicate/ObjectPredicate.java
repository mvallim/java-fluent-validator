 package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

public final class ObjectPredicate {

	private ObjectPredicate() {
		super();
	}

	/**
	 *
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
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(nullValue -> nullValue().test(source.apply(nullValue)));
	}

	/**
	 *
	 * @param obj
	 * @return
	 */
	public static <T> Predicate<T> equalTo(final T obj) {
		return PredicateBuilder.<T>from(not(nullValue())).and(equalTo -> equalTo.equals(obj));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @return
	 */
	public static <T> Predicate<T> equalTo(final Function<T, ?> source, final Function<T, ?> target) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(equalTo -> not(nullValue()).test(source.apply(equalTo)))
		        .and(equalTo -> not(nullValue()).test(target.apply(equalTo)))
		        .and(equalTo -> source.apply(equalTo).equals(target.apply(equalTo)));
	}

	/**
	 *
	 * @param clazz
	 * @return
	 */
	public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(instanceOf -> not(nullValue()).test(clazz))
		        .and(instanceOf -> clazz.isAssignableFrom(instanceOf.getClass()));
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
				.and(instanceOf -> not(nullValue()).test(source.apply(instanceOf)))
				.and(instanceOf -> not(nullValue()).test(clazz))
		        .and(instanceOf -> clazz.isAssignableFrom(source.apply(instanceOf).getClass()));
	}

}
