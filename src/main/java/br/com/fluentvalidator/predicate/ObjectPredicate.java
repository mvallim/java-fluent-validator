package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;

import java.util.Objects;
import java.util.function.Predicate;

@SuppressWarnings("all")
public final class ObjectPredicate {

	private ObjectPredicate() {
		super();
	}

	public static <E, T extends E> Predicate<T> nullValue() {
		return is(Objects::isNull);
	}

	public static <E, T extends E> Predicate<T> nullValue(final Class<T> clazz) {
		return nullValue();
	}
	
	public static <E, T extends E> Predicate<T> equalTo(final T obj) {
		return not((Predicate<T>)nullValue()).and(equalTo -> equalTo.equals(obj));
	}

	public static <E, T extends E> Predicate<T> instanceOf(final Class<?> clazz) {
		return not((Predicate<T>)nullValue()).and(clazz::isInstance);
	}

}
