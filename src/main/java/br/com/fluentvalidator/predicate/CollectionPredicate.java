package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;

import java.util.Collection;
import java.util.function.Predicate;

public final class CollectionPredicate {

	private static final Integer ZERO = 0;

	private CollectionPredicate() {
		super();
	}

	public static <T extends Collection<?>> Predicate<T> empty() {
		return is(empty -> ZERO.equals(empty.size()));
	}

	public static <T extends Collection<?>> Predicate<T> hasItems() {
		return is(not(empty -> ZERO.equals(empty.size())));
	}

	public static <T extends Collection<?>> Predicate<T> hasSize(final int size) {
		return is(empty -> size == empty.size());
	}

}
