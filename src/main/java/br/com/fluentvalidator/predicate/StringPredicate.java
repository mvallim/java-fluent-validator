package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Predicate;

public final class StringPredicate {

	private StringPredicate() {
		super();
	}

	public static Predicate<String> stringSizeGreaterThan(final int size) {
		return is(moreThan -> moreThan.length() > size);
	}

	public static Predicate<String> stringSizeLessThan(final int size) {
		return is(lessThan -> lessThan.length() < size);
	}

	public static Predicate<String> stringSizeGreaterThanOrEqual(final int size) {
		return is(moreThanOrEqual -> moreThanOrEqual.length() >= size);
	}

	public static Predicate<String> stringSizeLessThanOrEqual(final int size) {
		return is(lessThanOrEqual -> lessThanOrEqual.length() <= size);
	}

	public static Predicate<String> stringSizeBetween(final int minSize, final int maxSize) {
		return is(stringSizeGreaterThanOrEqual(minSize).and(stringSizeLessThanOrEqual(maxSize)));
	}

	public static Predicate<String> stringEmptyOrNull() {
		return is(nullValue(String.class)).or(String::isEmpty);
	}

	public static Predicate<String> stringContains(final String str) {
		return is(contains -> contains.contains(str));
	}

	public static Predicate<String> matches(final String regex) {
		return is(matches -> matches.matches(regex));
	}
}
