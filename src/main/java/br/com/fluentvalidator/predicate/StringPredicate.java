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
		return not(nullValue(String.class)).and(stringSizeGreaterThan -> stringSizeGreaterThan.length() > size);
	}

	public static Predicate<String> stringSizeLessThan(final int size) {
		return not(nullValue(String.class)).and(stringSizeLessThan -> stringSizeLessThan.length() < size);
	}

	public static Predicate<String> stringSizeGreaterThanOrEqual(final int size) {
		return not(nullValue(String.class)).and(stringSizeGreaterThanOrEqual -> stringSizeGreaterThanOrEqual.length() >= size);
	}

	public static Predicate<String> stringSizeLessThanOrEqual(final int size) {
		return not(nullValue(String.class)).and(stringSizeLessThanOrEqual -> stringSizeLessThanOrEqual.length() <= size);
	}

	public static Predicate<String> stringSizeBetween(final int minSize, final int maxSize) {
		return not(nullValue(String.class)).and(stringSizeBetween -> stringSizeBetween.length() > minSize && stringSizeBetween.length() < maxSize);
	}

	public static Predicate<String> stringEmptyOrNull() {
		return is(nullValue(String.class)).or(String::isEmpty);
	}

	public static Predicate<String> stringContains(final String str) {
		return not(nullValue(String.class)).and(stringContains -> stringContains.contains(str));
	}

	public static Predicate<String> stringMatches(final String regex) {
		return not(nullValue(String.class)).and(stringMatches -> stringMatches.matches(regex));
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
