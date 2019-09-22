package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Function;
import java.util.function.Predicate;

public final class ComparablePredicate {

    private static final Integer MINUS = -1;

    private static final Integer PLUS = 1;

    private static final Integer ZERO = 0;

    private ComparablePredicate() {
        super();
    }

    /**
     *
     * @param source
     * @param target
     * @return
     */
    public static <T, E extends Comparable<E>> Predicate<T> equalTo(final Function<T, E> source, final Function<T, E> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> equalTo(target.apply(obj)).test(source.apply(obj)));
    }

    /**
     *
     * @param source
     * @param target
     * @return
     */
    public static <T, E extends Comparable<E>> Predicate<T> lessThan(final Function<T, E> source, final Function<T, E> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> lessThan(target.apply(obj)).test(source.apply(obj)));
    }

    /**
     *
     * @param source
     * @param target
     * @return
     */
    public static <T, E extends Comparable<E>> Predicate<T> lessThanOrEqual(final Function<T, E> source, final Function<T, E> target) {
        return lessThan(source, target).or(equalTo(source, target));
    }

    /**
     *
     * @param source
     * @param target
     * @return
     */
    public static <T, E extends Comparable<E>> Predicate<T> greaterThan(final Function<T, E> source, final Function<T, E> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> greaterThan(target.apply(obj)).test(source.apply(obj)));
    }

    /**
     *
     * @param source
     * @param target
     * @return
     */
    public static <T, E extends Comparable<E>> Predicate<T> greaterThanOrEqual(final Function<T, E> source, final Function<T, E> target) {
        return greaterThan(source, target).or(equalTo(source, target));
    }

    /**
     *
     * @param value
     * @return
     */
    public static <E, T extends Comparable<E>> Predicate<T> equalTo(final E value) {
        return PredicateBuilder.<T>from(not(nullValue())).and(equalTo -> not(nullValue()).test(value)).and(equalTo -> ZERO.equals(equalTo.compareTo(value)));
    }

    /**
     *
     * @param max
     * @return
     */
    public static <E, T extends Comparable<E>> Predicate<T> lessThan(final E max) {
        return PredicateBuilder.<T>from(not(nullValue())).and(lessThan -> not(nullValue()).test(max)).and(lessThan -> MINUS.equals(lessThan.compareTo(max)));
    }

    /**
     *
     * @param min
     * @return
     */
    public static <E, T extends Comparable<E>> Predicate<T> greaterThan(final E min) {
        return PredicateBuilder.<T>from(not(nullValue())).and(greaterThan -> not(nullValue()).test(min)).and(greaterThan -> PLUS.equals(greaterThan.compareTo(min)));
    }

    /**
     *
     * @param min
     * @return
     */
    public static <E, T extends Comparable<E>> Predicate<T> greaterThanOrEqual(final E min) {
        return PredicateBuilder.<T>from(not(nullValue())).and(greaterThan(min).or(equalTo(min)));
    }

    /**
     *
     * @param max
     * @return
     */
    public static <E, T extends Comparable<E>> Predicate<T> lessThanOrEqual(final E max) {
        return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).or(equalTo(max)));
    }

    /**
     *
     * @param min
     * @param max
     * @return
     */
    public static <E, T extends Comparable<E>> Predicate<T> between(final E min, final E max) {
        return PredicateBuilder.<T>from(not(nullValue())).and(lessThan(max).and(greaterThan(min)));
    }

}
