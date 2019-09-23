package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

public final class ObjectPredicate {

	private ObjectPredicate() {
		super();
	}

	public static <T> Predicate<T> nullValue() {
		return PredicateBuilder.<T>from(is(Objects::isNull));
	}

	public static <T> Predicate<T> nullValue(final Function<T, ?> source) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(nullValue -> Objects.isNull(source.apply(nullValue)));
	}

	public static <T> Predicate<T> equalTo(final T obj) {
		return PredicateBuilder.<T>from(not(nullValue())).and(equalTo -> equalTo.equals(obj));
	}

	public static <T> Predicate<T> equalTo(final Function<T, ?> source, final Function<T, ?> target) {
		return PredicateBuilder.<T>from(not(nullValue()))
				.and(equalTo -> not(nullValue()).test(source.apply(equalTo)))
				.and(equalTo -> not(nullValue()).test(target.apply(equalTo)))
				.and(equalTo -> source.apply(equalTo).equals(target.apply(equalTo)));
	}

	public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
		return PredicateBuilder.<T>from(not(nullValue())).and(clazz::isInstance);
	}

}
