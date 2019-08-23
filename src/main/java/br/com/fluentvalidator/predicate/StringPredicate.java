package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Predicate;

public final class StringPredicate {

	private StringPredicate() {
		super();
	}

	public static Predicate<String> stringSizeGreaterThan(final int size) {
		return is(stringSizeGreaterThan -> {
			Assertions.checkNotNull(stringSizeGreaterThan, "stringSizeGreaterThan");
			return stringSizeGreaterThan.length() > size;
		});
	}

	public static Predicate<String> stringSizeLessThan(final int size) {
		return is(stringSizeLessThan -> {
			Assertions.checkNotNull(stringSizeLessThan, "stringSizeLessThan");
			return stringSizeLessThan.length() < size;
		});
	}

	public static Predicate<String> stringSizeGreaterThanOrEqual(final int size) {
		return is(stringSizeGreaterThanOrEqual -> {
			Assertions.checkNotNull(stringSizeGreaterThanOrEqual, "stringSizeGreaterThanOrEqual");
			return stringSizeGreaterThanOrEqual.length() >= size;
		});
	}

	public static Predicate<String> stringSizeLessThanOrEqual(final int size) {
		return is(stringSizeLessThanOrEqual -> {
			Assertions.checkNotNull(stringSizeLessThanOrEqual, "stringSizeLessThanOrEqual");
			return stringSizeLessThanOrEqual.length() <= size;
		});
	}

	public static Predicate<String> stringSizeBetween(final int minSize, final int maxSize) {
		return is(stringSizeBetween -> {
			Assertions.checkNotNull(stringSizeBetween, "stringSizeBetween");
			return stringSizeBetween.length() > minSize && stringSizeBetween.length() < maxSize;
		});
	}

	public static Predicate<String> stringEmptyOrNull() {
		return is(nullValue(String.class)).or(String::isEmpty);
	}

	public static Predicate<String> stringContains(final String str) {
		return is(stringContains -> {
			Assertions.checkNotNull(stringContains, "stringContains");
			return stringContains.contains(str);
		});
	}

	public static Predicate<String> stringMatches(final String regex) {
		return is(stringMatches -> {
			Assertions.checkNotNull(stringMatches, "stringMatches");
			return stringMatches.matches(regex);
		});
	}
}
