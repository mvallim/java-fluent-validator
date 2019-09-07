package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.*;
import static br.com.fluentvalidator.predicate.ObjectPredicate.*;

import java.util.function.Predicate;

@SuppressWarnings("unchecked")
public final class ComparablePredicate {

	private static final Integer MINUS = -1;
	
	private static final Integer PLUS = 1;
	
	private static final Integer ZERO = 0;
	
	private ComparablePredicate() {
		super();
	}

	public static <E, T extends Comparable<E>> Predicate<T> lessThan(final E max){
		return not((Predicate<T>)nullValue()).and(lessThan -> MINUS.equals(lessThan.compareTo(max)));
	}
	
	public static <E, T extends Comparable<E>> Predicate<T> greaterThan(final E min){
		return not((Predicate<T>)nullValue()).and(greaterThan -> PLUS.equals(greaterThan.compareTo(min)));
	}
	
	public static <E, T extends Comparable<E>> Predicate<T> greaterThanOrEqual(final E min){
		return not((Predicate<T>)nullValue()).and(greaterThanOrEqual -> PLUS.equals(greaterThanOrEqual.compareTo(min)) || (ZERO.equals(greaterThanOrEqual.compareTo(min))));
	}
	
	public static <E, T extends Comparable<E>> Predicate<T> lessThanOrEqual(final E max){
		return not((Predicate<T>)nullValue()).and(lessThanOrEqual -> MINUS.equals(lessThanOrEqual.compareTo(max)) || ZERO.equals(lessThanOrEqual.compareTo(max)));
	}	
	
	public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final E max){
		return not((Predicate<T>)nullValue()).and(between -> PLUS.equals(between.compareTo(min)) && MINUS.equals(between.compareTo(max)));
	}
	
}
