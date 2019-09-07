package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Predicate;

public final class CollectionPredicate {

	private CollectionPredicate() {
		super();
	}

	public static <E, T extends Collection<E>> Predicate<T> empty() {
		return SimplePredicate.<T>from(not(nullValue())).and(Collection::isEmpty);
	}

	public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
		return SimplePredicate.<T>from(not(nullValue())).and(hasItem -> hasItem.contains(object));
	}
	
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
		return SimplePredicate.<T>from(not(nullValue())).and(hasItems -> hasItems.containsAll(objects));
	}

	@SafeVarargs
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final E... objects) {
		return is(hasItems(Arrays.asList(objects)));
	}
	
	public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
		return SimplePredicate.<T>from(not(nullValue())).and(hasItems -> {
			for (final Object object : objects) {
				if (hasItems.contains(object)) return true;
			}
			return false;
		});
	}
	
	@SafeVarargs
	public static <E, T extends Collection<E>> Predicate<T> hasAny(final E... objects) {
		return is(hasAny(Arrays.asList(objects)));
	}	

	public static <E, T extends Collection<E>> Predicate<T> hasSize(final int size) {
		return SimplePredicate.<T>from(not(nullValue())).and(hasSize -> hasSize.size() == size);
	}

}
