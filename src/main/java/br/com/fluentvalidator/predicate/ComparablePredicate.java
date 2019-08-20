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
		return is(lessThan -> MINUS.equals(lessThan.compareTo(max)));
	}
	
	public static <T extends Comparable<T>> Predicate<T> greaterThan(final T min){
		return is(greaterThan -> PLUS.equals(greaterThan.compareTo(min)));
	}
	
	public static <T extends Comparable<T>> Predicate<T> greaterThanOrEqual(final T min){
		return is(greaterThan(min).or(greaterThanOrEqual -> ZERO.equals(greaterThanOrEqual.compareTo(min))));
	}
	
	public static <T extends Comparable<T>> Predicate<T> lessThanOrEqual(final T max){
		return is(lessThan(max).or(lessThanOrEqual -> ZERO.equals(lessThanOrEqual.compareTo(max))));
	}	
	
	public static <T extends Comparable<T>> Predicate<T> between(final T min, final T max){
		return is(greaterThan(min).and(lessThan(max)));
	}
	
}
