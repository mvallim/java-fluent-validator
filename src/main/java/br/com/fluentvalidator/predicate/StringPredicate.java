package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ComparablePredicate.between;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.math.BigDecimal;
import java.util.function.Predicate;

public final class StringPredicate {

	private StringPredicate() {
		super();
	}

	public static Predicate<String> stringSize(final int size) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringSize -> equalTo(size).test(stringSize.length()));
	}

	public static Predicate<String> stringSizeGreaterThan(final int size) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringSizeGreaterThan -> greaterThan(size).test(stringSizeGreaterThan.length()));
	}

	public static Predicate<String> stringSizeLessThan(final int size) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringSizeLessThan -> lessThan(size).test(stringSizeLessThan.length()));
	}

	public static Predicate<String> stringSizeGreaterThanOrEqual(final int size) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringSizeGreaterThanOrEqual -> greaterThanOrEqual(size).test(stringSizeGreaterThanOrEqual.length()));
	}

	public static Predicate<String> stringSizeLessThanOrEqual(final int size) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringSizeLessThanOrEqual -> lessThanOrEqual(size).test(stringSizeLessThanOrEqual.length()));
	}

	public static Predicate<String> stringSizeBetween(final int minSize, final int maxSize) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringSizeBetween -> between(minSize, maxSize).test(stringSizeBetween.length()));
	}

	public static Predicate<String> stringEmptyOrNull() {
		return PredicateBuilder.<String>from(is(nullValue()))
				.or(String::isEmpty);
	}

	public static Predicate<String> stringContains(final String str) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringContains -> stringContains.contains(str));
	}

	public static Predicate<String> stringMatches(final String regex) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(stringMatches -> stringMatches.matches(regex));
	}
	
	public static Predicate<String> isNumeric() {
		return not(stringEmptyOrNull())
				.and(isNumeric -> isNumeric.chars().allMatch(Character::isDigit));
	}
	
	public static Predicate<String> isAlpha() {
		return not(stringEmptyOrNull())
				.and(isNumeric -> isNumeric.chars().allMatch(Character::isLetter));
	}
	
	public static Predicate<String> isAlphaNumeric() {
		return not(stringEmptyOrNull())
				.and(isNumeric -> isNumeric.chars().allMatch(Character::isLetterOrDigit));
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
