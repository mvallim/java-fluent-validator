package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Predicate;

public final class StringPredicate {

	private StringPredicate() {
		super();
	}

	public static Predicate<String> siszeGreaterThan(final int size) {
		return is(moreThan -> moreThan.length() > size);
	}

	public static Predicate<String> sizeLessThan(final int size) {
		return is(lessThan -> lessThan.length() < size);
	}

	public static Predicate<String> sizeGreaterThanOrEqual(final int size) {
		return is(moreThanOrEqual -> moreThanOrEqual.length() >= size);
	}

	public static Predicate<String> sizeLessThanOrEqual(final int size) {
		return is(lessThanOrEqual -> lessThanOrEqual.length() <= size);
	}

	public static Predicate<String> sizeBetween(final int minSize, final int maxSize) {
		return is(sizeGreaterThanOrEqual(minSize).and(sizeLessThanOrEqual(maxSize)));
	}

	public static Predicate<String> emptyOrNullString() {
		return is(nullValue(String.class)).or(String::isEmpty);
	}

	public static Predicate<String> containsString(final String str) {
		return is(contains -> contains.contains(str));
	}

	public static Predicate<String> matches(final String regex) {
		return is(matches -> matches.matches(regex));
	}
}
