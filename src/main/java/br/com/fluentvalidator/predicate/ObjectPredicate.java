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
	 * @param <T>
	 * @return
	 */
	public static <T> Predicate<T> nullValue() {
		return PredicateBuilder.<T>from(Objects::isNull);
	}

	/**
	 * 
	 * @param <T>
	 * @param obj
	 * @return
	 */
	public static <T> Predicate<T> equalTo(final T obj) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(equalTo -> equalTo.equals(obj));
	}

	/**
	 * 
	 * @param <T>
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
	 * @return
	 */
	public static <T> Predicate<T> nullValue(final Function<T, ?> source) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(obj -> nullValue().test(source.apply(obj)));
	}
	
	/**
	 * 
	 * @param <T>
	 * @param source
	 * @param target
	 * @return
	 */
	public static <T> Predicate<T> equalTo(final Function<T, Object> source, final Function<T, Object> target) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(obj -> not(nullValue()).test(source.apply(obj)))
				.and(obj -> not(nullValue()).test(target.apply(obj)))
				.and(obj -> equalTo(source.apply(obj)).test(target.apply(obj)));
	}
	
	/**
	 *
	 * @param <T>
	 * @param source
	 * @param clazz
	 * @return
	 */ 
	public static <T> Predicate<T> instanceOf(final Function<T, Object> source, final Class<?> clazz) {
		return PredicateBuilder.<T>from(not(nullValue()))
		        .and(instanceOf -> instanceOf(clazz).test(source.apply(instanceOf)));
	}

}
