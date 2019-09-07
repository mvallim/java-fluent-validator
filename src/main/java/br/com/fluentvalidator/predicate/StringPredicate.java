package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.math.BigDecimal;
import java.util.function.Predicate;

public final class StringPredicate {

	private StringPredicate() {
		super();
	}

	public static Predicate<String> stringSizeGreaterThan(final int size) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringSizeGreaterThan -> stringSizeGreaterThan.length() > size);
	}

	public static Predicate<String> stringSizeLessThan(final int size) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringSizeLessThan -> stringSizeLessThan.length() < size);
	}

	public static Predicate<String> stringSizeGreaterThanOrEqual(final int size) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringSizeGreaterThanOrEqual -> stringSizeGreaterThanOrEqual.length() >= size);
	}

	public static Predicate<String> stringSizeLessThanOrEqual(final int size) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringSizeLessThanOrEqual -> stringSizeLessThanOrEqual.length() <= size);
	}

	public static Predicate<String> stringSizeBetween(final int minSize, final int maxSize) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringSizeBetween -> stringSizeBetween.length() > minSize && stringSizeBetween.length() < maxSize);
	}

	public static Predicate<String> stringEmptyOrNull() {
		return SimplePredicate.<String>from(is(nullValue()))
				.or(String::isEmpty);
	}

	public static Predicate<String> stringContains(final String str) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringContains -> stringContains.contains(str));
	}

	public static Predicate<String> stringMatches(final String regex) {
		return SimplePredicate.<String>from(not(nullValue()))
				.and(stringMatches -> stringMatches.matches(regex));
	}
	
	public static Predicate<String> isNumeric() {
		return not(stringEmptyOrNull()).and(isNumeric -> isNumeric.chars().allMatch(Character::isDigit));
	}
	
	public static Predicate<String> isAlpha() {
		return not(stringEmptyOrNull()).and(isNumeric -> isNumeric.chars().allMatch(Character::isLetter));
	}
	
	public static Predicate<String> isAlphaNumeric() {
		return not(stringEmptyOrNull()).and(isNumeric -> isNumeric.chars().allMatch(Character::isLetterOrDigit));
	}
	
	public static Predicate<String> isNumber() {
		return not(stringEmptyOrNull()).and(isNumber -> {
			try {
				new BigDecimal(isNumber);
			} catch(final NumberFormatException e) {
				return false;
			}
			return true;
		});
	}

}
