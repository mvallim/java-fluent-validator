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
		return is(Collection::isEmpty);
	}

	public static <T extends Collection<?>> Predicate<T> hasItem(final Object object) {
		return is(hasItem -> hasItem.contains(object));
	}
	
	public static <T extends Collection<?>> Predicate<T> hasItems(final Collection<Object> objects) {
		return is(hasItems -> hasItems.containsAll(objects));
	}

	public static <T extends Collection<?>> Predicate<T> hasItems(final Object... objects) {
		return is(hasItems(Arrays.asList(objects)));
	}
	
	public static <T extends Collection<?>> Predicate<T> hasAny(final Collection<Object> objects) {
		return is(hasItems -> { 
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
		return is(empty -> size == empty.size());
	}

}
