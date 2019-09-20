package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.function.Predicate;

public final class DateTimePredicate {

	private DateTimePredicate() {
		super();
	}
	
	public static Predicate<String> dateTimeEqualTo(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateTimeEqualTo -> not(stringEmptyOrNull()).test(dateString))
				.and(dateTimeEqualTo -> not(stringEmptyOrNull()).test(pattern))
				.and(dateTimeEqualTo -> {
					try {
						final SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
						final Date dateTest = dateFormat.parse(dateTimeEqualTo);
						final Date date = dateFormat.parse(dateString);			
						return dateTest.equals(date);						
					} catch (final ParseException e) {
						throw new IllegalArgumentException(e);
					}
				});
	}

	public static Predicate<String> dateTimeGreaterThan(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateTimeGreaterThan -> not(stringEmptyOrNull()).test(dateString))
				.and(dateTimeGreaterThan -> not(stringEmptyOrNull()).test(pattern))
				.and(dateTimeGreaterThan -> {
					try {
						final SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
						final Date dateTest = dateFormat.parse(dateTimeGreaterThan);
						final Date date = dateFormat.parse(dateString);			
						return dateTest.after(date);						
					} catch (final ParseException e) {
						throw new IllegalArgumentException(e);
					}
				});
	}

	public static Predicate<String> dateTimeLessThan(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateTimeLessThan -> not(stringEmptyOrNull()).test(dateString))
				.and(dateTimeLessThan -> not(stringEmptyOrNull()).test(pattern))
				.and(dateTimeLessThan -> {
					try {
						final SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
						final Date dateTest = dateFormat.parse(dateTimeLessThan);
						final Date date = dateFormat.parse(dateString);			
						return dateTest.before(date);
					} catch (final ParseException e) {
						throw new IllegalArgumentException(e);
					}
				});
	}

	public static Predicate<String> dateTimeGreaterThanOrEqual(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateTimeGreaterThanOrEqual -> not(stringEmptyOrNull()).test(dateString))
				.and(dateTimeGreaterThanOrEqual -> not(stringEmptyOrNull()).test(pattern))
				.and(dateTimeGreaterThan(dateString, pattern).or(dateTimeEqualTo(dateString, pattern)));
	}

	public static Predicate<String> dateTimeLessThanOrEqual(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateTimeLessThanOrEqual -> not(stringEmptyOrNull()).test(dateString))
				.and(dateTimeLessThanOrEqual -> not(stringEmptyOrNull()).test(pattern))
				.and(dateTimeLessThan(dateString, pattern).or(dateTimeEqualTo(dateString, pattern)));
	}

	public static Predicate<String> dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue()))
				.and(dateTimeBetween -> not(stringEmptyOrNull()).test(dateStringMin))
				.and(dateTimeBetween -> not(stringEmptyOrNull()).test(dateStringMax))
				.and(dateTimeBetween -> not(stringEmptyOrNull()).test(pattern))
				.and(dateTimeLessThanOrEqual(dateStringMax, pattern))
				.and(dateTimeGreaterThanOrEqual(dateStringMin, pattern));
	}
	
}