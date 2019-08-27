package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Predicate;

public final class CollectionPredicate {

	private CollectionPredicate() {
		super();
	}

	public static <E, T extends Collection<E>> Predicate<T> empty() {
		return is(empty -> {
			Assertions.checkNotNull(empty, "empty");
			return empty.isEmpty();
		});
	}

	public static <E, T extends Collection<E>> Predicate<T> hasItem(final E object) {
		return is(hasItem -> {
			Assertions.checkNotNull(hasItem, "hasItem");
			return hasItem.contains(object);
		});
	}
	
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final Collection<E> objects) {
		return is(hasItems -> {
			Assertions.checkNotNull(hasItems, "hasItems");
			return hasItems.containsAll(objects);
		});
	}

	@SafeVarargs
	public static <E, T extends Collection<E>> Predicate<T> hasItems(final E... objects) {
		return is(hasItems(Arrays.asList(objects)));
	}
	
	public static <E, T extends Collection<E>> Predicate<T> hasAny(final Collection<E> objects) {
		return is(hasItems -> {
			Assertions.checkNotNull(hasItems, "hasAny");
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
		return is(hasSize -> {
			Assertions.checkNotNull(hasSize, "hasSize");
			return hasSize.size() == size;
		});
	}

}
