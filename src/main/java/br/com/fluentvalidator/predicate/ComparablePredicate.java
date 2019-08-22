package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;

import java.util.function.Predicate;

public final class ComparablePredicate {

	private static final Integer MINUS = -1;
	
	private static final Integer PLUS = 1;
	
	private static final Integer ZERO = 0;
	
	private ComparablePredicate() {
		super();
	}

	public static <T extends Comparable<T>> Predicate<T> lessThan(final T max){
		return is(lessThan -> {
			Assertions.checkNotNull(lessThan, "lessThan");
			return MINUS.equals(lessThan.compareTo(max));
		});
	}
	
	public static <T extends Comparable<T>> Predicate<T> greaterThan(final T min){
		return is(greaterThan -> {
			Assertions.checkNotNull(greaterThan, "greaterThan");
			return PLUS.equals(greaterThan.compareTo(min));
		});
	}
	
	public static <T extends Comparable<T>> Predicate<T> greaterThanOrEqual(final T min){
		return is(greaterThan(min).or(greaterThanOrEqual -> {
			Assertions.checkNotNull(greaterThanOrEqual, "greaterThanOrEqual");
			return ZERO.equals(greaterThanOrEqual.compareTo(min));
		}));
	}
	
	public static <T extends Comparable<T>> Predicate<T> lessThanOrEqual(final T max){
		return is(lessThan(max).or(lessThanOrEqual -> {
			Assertions.checkNotNull(lessThanOrEqual, "lessThanOrEqual");
			return ZERO.equals(lessThanOrEqual.compareTo(max));
		}));
	}	
	
	public static <T extends Comparable<T>> Predicate<T> between(final T min, final T max){
		return is(greaterThan(min).and(lessThan(max)));
	}
	
}
