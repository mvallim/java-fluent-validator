package br.com.fluentvalidator.predicate;

import java.util.function.Predicate;

public final class SimplePredicate<T> implements Predicate<T> {

	private Predicate<T> predicate;

	private SimplePredicate(final Predicate<T> predicate) {
		this.predicate = predicate;
	}

	public static <T> SimplePredicate<T> from(final Predicate<T> predicate) {
		return new SimplePredicate<>(predicate);
	}

	@Override
	public boolean test(final T value) {
		return predicate.test(value);
	}

}
