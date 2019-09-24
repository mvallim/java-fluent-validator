package br.com.fluentvalidator.predicate;

import java.util.function.Predicate;

public final class PredicateBuilder<T> implements Predicate<T> {

	private final Predicate<T> predicate;

	private PredicateBuilder(final Predicate<T> predicate) {
		this.predicate = predicate;
	}

	/**
	 *
	 * @param <T>
	 * @param predicate
	 * @return
	 */
	public static <T> Predicate<T> from(final Predicate<T> predicate) {
		return new PredicateBuilder<>(predicate);
	}

	@Override
	public boolean test(final T value) {
		return predicate.test(value);
	}

}
