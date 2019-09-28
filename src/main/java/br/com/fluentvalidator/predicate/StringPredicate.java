package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.math.BigDecimal;
import java.util.function.Function;
import java.util.function.Predicate;

public final class StringPredicate {

    private StringPredicate() {
        super();
    }

    /**
     *
     * @param source
     * @param target
     * @return
     */
    public static <T> Predicate<T> equalsIgnoreCase(final Function<T, String> source, final Function<T, String> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> equalsIgnoreCase(source, target.apply(obj)).test(obj));
    }

    /**
     *
     * @param source
     * @param value
     * @return
     */
    public static <T> Predicate<T> equalsIgnoreCase(final Function<T, String> source, final String value) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(source.apply(obj))).and(obj -> equalsIgnoreCase(value).test(source.apply(obj)));
    }

    /**
     *
     * @param value
     * @return
     */
    public static Predicate<String> equalsIgnoreCase(final String value) {
        return PredicateBuilder.<String>from(not(nullValue())).and(obj -> not(nullValue()).test(value)).and(obj -> obj.equalsIgnoreCase(value));
    }

    /**
     *
     * @return
     */
    public static Predicate<String> isAlpha() {
        return PredicateBuilder.<String>from(not(stringEmptyOrNull())).and(isNumeric -> isNumeric.chars().allMatch(Character::isLetter));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @return
     */
    public static <T> Predicate<T> isAlpha(final Function<T, String> source) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isAlpha().test(source.apply(obj)));
    }

    /**
     *
     * @return
     */
    public static Predicate<String> isAlphaNumeric() {
        return PredicateBuilder.<String>from(not(stringEmptyOrNull())).and(isNumeric -> isNumeric.chars().allMatch(Character::isLetterOrDigit));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @return
     */
    public static <T> Predicate<T> isAlphaNumeric(final Function<T, String> source) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isAlphaNumeric().test(source.apply(obj)));
    }

    /**
     *
     * @return
     */
    public static Predicate<String> isNumber() {
        return PredicateBuilder.<String>from(not(stringEmptyOrNull())).and(isNumber -> {
            try {
                new BigDecimal(isNumber);
            } catch (final NumberFormatException e) {
                return false;
            }
            return true;
        });
    }

    /**
     *
     * @param        <T>
     * @param source
     * @return
     */
    public static <T> Predicate<T> isNumber(final Function<T, String> source) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isNumber().test(source.apply(obj)));
    }

    /**
     *
     * @return
     */
    public static Predicate<String> isNumeric() {
        return PredicateBuilder.<String>from(not(stringEmptyOrNull())).and(isNumeric -> isNumeric.chars().allMatch(Character::isDigit));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @return
     */
    public static <T> Predicate<T> isNumeric(final Function<T, String> source) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isNumeric().test(source.apply(obj)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param str
     * @return
     */
    public static <T> Predicate<T> stringContains(final Function<T, String> source, final String str) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringContains(str).test(source.apply(obj)));
    }

    /**
     *
     * @param str
     * @return
     */
    public static Predicate<String> stringContains(final String str) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringContains -> not(nullValue()).test(str)).and(stringContains -> stringContains.contains(str));
    }

    /**
     *
     * @return
     */
    public static Predicate<String> stringEmptyOrNull() {
        return PredicateBuilder.<String>from(is(nullValue())).or(String::isEmpty);
    }

    /**
     *
     * @param        <T>
     * @param source
     * @return
     */
    public static <T> Predicate<T> stringEmptyOrNull(final Function<T, String> source) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringEmptyOrNull().test(source.apply(obj)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param regex
     * @return
     */
    public static <T> Predicate<T> stringMatches(final Function<T, String> source, final String regex) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringMatches(regex).test(source.apply(obj)));
    }

    /**
     *
     * @param regex
     * @return
     */
    public static Predicate<String> stringMatches(final String regex) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringMatches -> not(nullValue()).test(regex)).and(stringMatches -> stringMatches.matches(regex));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param target
     * @return
     */
    public static <T> Predicate<T> stringSize(final Function<T, String> source, final Function<T, String> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(target.apply(obj)))
                .and(obj -> stringSize(target.apply(obj).length()).test(source.apply(obj)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param size
     * @return
     */
    public static <T> Predicate<T> stringSize(final Function<T, String> source, final Integer size) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringSize(size).test(source.apply(obj)));
    }

    /**
     *
     * @param size
     * @return
     */
    public static Predicate<String> stringSize(final Integer size) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringSize -> not(nullValue()).test(size)).and(stringSize -> equalTo(size).test(stringSize.length()));
    }

    /**
     *
     * @param         <T>
     * @param source
     * @param minSize
     * @param maxSize
     * @return
     */
    public static <T> Predicate<T> stringSizeBetween(final Function<T, String> source, final Integer minSize, final Integer maxSize) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringSizeBetween(minSize, maxSize).test(source.apply(obj)));
    }

    /**
     *
     * @param minSize
     * @param maxSize
     * @return
     */
    public static Predicate<String> stringSizeBetween(final Integer minSize, final Integer maxSize) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringSizeGreaterThanOrEqual(minSize).and(stringSizeLessThanOrEqual(maxSize)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param target
     * @return
     */
    public static <T> Predicate<T> stringSizeGreaterThan(final Function<T, String> source, final Function<T, String> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(target.apply(obj)))
                .and(obj -> stringSizeGreaterThan(target.apply(obj).length()).test(source.apply(obj)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param size
     * @return
     */
    public static <T> Predicate<T> stringSizeGreaterThan(final Function<T, String> source, final Integer size) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringSizeGreaterThan(size).test(source.apply(obj)));
    }

    /**
     *
     * @param size
     * @return
     */
    public static Predicate<String> stringSizeGreaterThan(final Integer size) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringSizeGreaterThan -> not(nullValue()).test(size))
                .and(stringSizeGreaterThan -> greaterThan(size).test(stringSizeGreaterThan.length()));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param target
     * @return
     */
    public static <T> Predicate<T> stringSizeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(stringSizeGreaterThan(source, target).or(stringSize(source, target)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param size
     * @return
     */
    public static <T> Predicate<T> stringSizeGreaterThanOrEqual(final Function<T, String> source, final Integer size) {
        return PredicateBuilder.<T>from(not(nullValue())).and(stringSizeGreaterThan(source, size).or(stringSize(source, size)));
    }

    /**
     *
     * @param size
     * @return
     */
    public static Predicate<String> stringSizeGreaterThanOrEqual(final Integer size) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringSizeGreaterThan(size).or(stringSize(size)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param target
     * @return
     */
    public static <T> Predicate<T> stringSizeLessThan(final Function<T, String> source, final Function<T, String> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> not(nullValue()).test(target.apply(obj)))
                .and(obj -> stringSizeLessThan(target.apply(obj).length()).test(source.apply(obj)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param size
     * @return
     */
    public static <T> Predicate<T> stringSizeLessThan(final Function<T, String> source, final Integer size) {
        return PredicateBuilder.<T>from(not(nullValue())).and(obj -> stringSizeLessThan(size).test(source.apply(obj)));
    }

    /**
     *
     * @param size
     * @return
     */
    public static Predicate<String> stringSizeLessThan(final Integer size) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringSizeLessThan -> not(nullValue()).test(size))
                .and(stringSizeLessThan -> lessThan(size).test(stringSizeLessThan.length()));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param target
     * @return
     */
    public static <T> Predicate<T> stringSizeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target) {
        return PredicateBuilder.<T>from(not(nullValue())).and(stringSizeLessThan(source, target).or(stringSize(source, target)));
    }

    /**
     *
     * @param        <T>
     * @param source
     * @param size
     * @return
     */
    public static <T> Predicate<T> stringSizeLessThanOrEqual(final Function<T, String> source, final Integer size) {
        return PredicateBuilder.<T>from(not(nullValue())).and(stringSizeLessThan(source, size).or(stringSize(source, size)));
    }

    /**
     *
     * @param size
     * @return
     */
    public static Predicate<String> stringSizeLessThanOrEqual(final Integer size) {
        return PredicateBuilder.<String>from(not(nullValue())).and(stringSizeLessThan(size).or(stringSize(size)));
    }

}
