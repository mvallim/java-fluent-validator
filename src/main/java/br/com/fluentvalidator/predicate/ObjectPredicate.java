package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;

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
		return nullValue();
	}
	
	public static <T> Predicate<T> equalTo(final T obj) {
		return is(equalTo -> {
			Assertions.checkNotNull(equalTo, "equalTo");
			return equalTo.equals(obj);
		});
	}

	public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
		return is(instanceOf -> {
			Assertions.checkNotNull(instanceOf, "instanceOf");
			return clazz.isInstance(instanceOf);
		});
	}

}
