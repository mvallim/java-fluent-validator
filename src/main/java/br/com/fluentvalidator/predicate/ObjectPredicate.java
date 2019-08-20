package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.*;

import java.util.Objects;
import java.util.function.Predicate;

public final class ObjectPredicate {

	private ObjectPredicate() {
		super();
	}

	public static <T> Predicate<T> nullValue() {
		return is(Objects::isNull);
	}
	
	@SuppressWarnings("all")
	public static <T> Predicate<T> nullValue(final Class<T> clazz) {
		return is(Objects::isNull);
	}
	
	public static <T> Predicate<T> equalTo(final T obj) {
		return is(obj::equals);
	}
	
	public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
		return is(clazz::isInstance);
	}

}
