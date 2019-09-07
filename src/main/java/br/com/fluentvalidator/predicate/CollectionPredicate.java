package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Predicate;

@SuppressWarnings("unchecked")
public final class CollectionPredicate {

	private CollectionPredicate() {
		super();
	}

	public static <E, T extends Collection<E>> Predicate<T> empty() {
		return not((Predicate<T>)nullValue()).and(Collection::isEmpty);
	}

	public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
		return not((Predicate<T>)nullValue()).and(hasItem -> hasItem.contains(object));
	}
	
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
		return not((Predicate<T>)nullValue()).and(hasItems -> hasItems.containsAll(objects));
	}

	@SafeVarargs
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final E... objects) {
		return is(hasItems(Arrays.asList(objects)));
	}
	
	public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
		return not((Predicate<T>)nullValue()).and(hasItems -> {
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
		return not((Predicate<T>)nullValue()).and(hasSize -> hasSize.size() == size);
	}

}
