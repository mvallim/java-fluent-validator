package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.function.Function;
import java.util.function.Predicate;

public final class DateTimePredicate {

	private DateTimePredicate() {
		super();
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeEqualTo(target.apply(obj), pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeGreaterThan(target.apply(obj), pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeLessThan(target.apply(obj), pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(dateTimeGreaterThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(dateTimeLessThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeEqualTo(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeEqualTo(target, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeGreaterThan(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeGreaterThan(target, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeLessThan(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeLessThan(target, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(dateTimeGreaterThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeLessThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(dateTimeLessThan(source, target, pattern).or(dateTimeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param dateStringMin
	 * @param dateStringMax
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> dateTimeBetween(final Function<T, String> source, final String dateStringMin, final String dateStringMax, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> dateTimeBetween(dateStringMin, dateStringMax, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param dateString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> dateTimeEqualTo(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue())).and(dateTimeEqualTo -> not(stringEmptyOrNull()).test(dateString))
		        .and(dateTimeEqualTo -> not(stringEmptyOrNull()).test(pattern)).and(dateTimeEqualTo -> {
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

	/**
	 *
	 * @param dateString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> dateTimeGreaterThan(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue())).and(dateTimeGreaterThan -> not(stringEmptyOrNull()).test(dateString))
		        .and(dateTimeGreaterThan -> not(stringEmptyOrNull()).test(pattern)).and(dateTimeGreaterThan -> {
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

	/**
	 *
	 * @param dateString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> dateTimeLessThan(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue())).and(dateTimeLessThan -> not(stringEmptyOrNull()).test(dateString))
		        .and(dateTimeLessThan -> not(stringEmptyOrNull()).test(pattern)).and(dateTimeLessThan -> {
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

	/**
	 *
	 * @param dateString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> dateTimeGreaterThanOrEqual(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(dateTimeGreaterThan(dateString, pattern).or(dateTimeEqualTo(dateString, pattern)));
	}

	/**
	 *
	 * @param dateString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> dateTimeLessThanOrEqual(final String dateString, final String pattern) {
		return PredicateBuilder.<String>from(dateTimeLessThan(dateString, pattern).or(dateTimeEqualTo(dateString, pattern)));
	}

	/**
	 *
	 * @param dateStringMin
	 * @param dateStringMax
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern) {
		return PredicateBuilder.<String>from(dateTimeLessThanOrEqual(dateStringMax, pattern).and(dateTimeGreaterThanOrEqual(dateStringMin, pattern)));
	}

}