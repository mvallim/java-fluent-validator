package br.com.fluentvalidator.predicate;

import java.util.function.Predicate;

public final class LogicalPredicate {
	
	private LogicalPredicate() {
		super();
	}
	
	static <T> Predicate<T> is(final Predicate<T> predicate) {
		return SimplePredicate.<T>from(predicate.and(is -> true));
	}

	public static <T> Predicate<T> not(final Predicate<T> predicate) {
		return SimplePredicate.<T>from(is(predicate).negate());
	}

	public static <T> Predicate<T> isTrue() {
		return SimplePredicate.<T>from(is(isTrue -> true));
	}
	
	public static <T> Predicate<T> isFalse() {
		return SimplePredicate.<T>from(is(isFalse -> false));
	}

}
