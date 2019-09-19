package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.function.Predicate;

public final class DatePredicate {

	private DatePredicate() {
		super();
	}
	
	public static Predicate<String> dateEqualTo(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateGreaterThan -> not(stringEmptyOrNull()).test(dateString))
				.and(dateGreaterThan -> not(stringEmptyOrNull()).test(pattern))
				.and(dateGreaterThan -> {
					try {
						final SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
						final Date dateTest = dateFormat.parse(dateGreaterThan);
						final Date date = dateFormat.parse(dateString);
						return dateTest.equals(date);
					} catch (ParseException e) {
						throw new IllegalArgumentException(e);
					}
				});
	}

	public static Predicate<String> dateGreaterThan(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateGreaterThan -> not(stringEmptyOrNull()).test(dateString))
				.and(dateGreaterThan -> not(stringEmptyOrNull()).test(pattern))
				.and(dateGreaterThan -> {
					try {
						final SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
						final Date dateTest = dateFormat.parse(dateGreaterThan);
						final Date date = dateFormat.parse(dateString);
						return dateTest.after(date);
					} catch (ParseException e) {
						throw new IllegalArgumentException(e);
					}
				});
	}

	public static Predicate<String> dateLessThan(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateLessThan -> not(stringEmptyOrNull()).test(dateString))
				.and(dateLessThan -> not(stringEmptyOrNull()).test(pattern))
				.and(dateLessThan -> {
					try {
						final SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
						final Date dateTest = dateFormat.parse(dateLessThan);
						final Date date = dateFormat.parse(dateString);
						return dateTest.before(date);
					} catch (final ParseException e) {
						throw new IllegalArgumentException(e);
					}
				});
	}

	public static Predicate<String> dateGreaterThanOrEqual(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateGreaterThanOrEqual -> not(stringEmptyOrNull()).test(dateString))
				.and(dateGreaterThanOrEqual -> not(stringEmptyOrNull()).test(pattern))
				.and(dateGreaterThan(dateString, pattern).or(dateEqualTo(dateString, pattern)));
	}

	public static Predicate<String> dateLessThanOrEqual(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateLessThanOrEqual -> not(stringEmptyOrNull()).test(dateString))
				.and(dateLessThanOrEqual -> not(stringEmptyOrNull()).test(pattern))
				.and(dateLessThan(dateString, pattern).or(dateEqualTo(dateString, pattern)));
	}

	public static Predicate<String> dateBetween(final String dateStringMin, final String dateStringMax, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateSizeGreaterThan -> not(stringEmptyOrNull()).test(dateStringMin))
				.and(dateSizeGreaterThan -> not(stringEmptyOrNull()).test(dateStringMax))
				.and(dateSizeGreaterThan -> not(stringEmptyOrNull()).test(pattern))
				.and(dateLessThanOrEqual(dateStringMax, pattern))
				.and(dateGreaterThanOrEqual(dateStringMin, pattern));
	}
	
}
