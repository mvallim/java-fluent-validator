package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Predicate;

public final class CollectionPredicate {

	private CollectionPredicate() {
		super();
	}

	public static <T extends Collection<?>> Predicate<T> empty() {
		return is(empty -> {
			Assertions.checkNotNull(empty, "empty");
			return empty.isEmpty();
		});
	}

	public static <T extends Collection<?>> Predicate<T> hasItem(final Object object) {
		return is(hasItem -> {
			Assertions.checkNotNull(hasItem, "hasItem");
			return hasItem.contains(object);
		});
	}
	
	public static <T extends Collection<?>> Predicate<T> hasItems(final Collection<?> objects) {
		return is(hasItems -> {
			Assertions.checkNotNull(hasItems, "hasItems");
			return hasItems.containsAll(objects);
		});
	}

	public static <T extends Collection<?>> Predicate<T> hasItems(final Object... objects) {
		return is(hasItems(Arrays.asList(objects)));
	}
	
	public static <T extends Collection<?>> Predicate<T> hasAny(final Collection<?> objects) {
		return is(hasItems -> {
			Assertions.checkNotNull(hasItems, "hasAny");
			for (final Object object : objects) {
				if (hasItems.contains(object)) return true;
			}
			return false;
		});
	}
	
	public static <T extends Collection<?>> Predicate<T> hasAny(final Object... objects) {
		return is(hasAny(Arrays.asList(objects)));
	}	

	public static <T extends Collection<?>> Predicate<T> hasSize(final int size) {
		return is(hasSize -> {
			Assertions.checkNotNull(hasSize, "hasSize");
			return hasSize.size() == size;
		});
	}

}
