package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;
import java.util.function.Predicate;

public final class TimePredicate {

	private TimePredicate() {
		super();
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param timeStringMin
	 * @param timeStringMax
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeBetween(final Function<T, String> source, final String timeStringMin, final String timeStringMax, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeBetween(timeStringMin, timeStringMax, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param timeStringMin
	 * @param timeStringMax
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> timeBetween(final String timeStringMin, final String timeStringMax, final String pattern) {
		return PredicateBuilder.<String>from(timeLessThanOrEqual(timeStringMax, pattern).and(timeGreaterThanOrEqual(timeStringMin, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeEqualTo(target.apply(obj), pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeEqualTo(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeEqualTo(target, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param timeString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> timeEqualTo(final String timeString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue())).and(timeEqualTo -> not(stringEmptyOrNull()).test(timeString))
		        .and(timeEqualTo -> not(stringEmptyOrNull()).test(pattern)).and(timeEqualTo -> {
			        try {
				        final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern(pattern);
				        final LocalTime timeTest = LocalTime.parse(timeEqualTo, timeFormat);
				        final LocalTime time = LocalTime.parse(timeString, timeFormat);
				        return timeTest.equals(time);
			        } catch (final IllegalArgumentException | DateTimeParseException ex) {
				        return false;
			        }
		        });
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeGreaterThan(target.apply(obj), pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeGreaterThan(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeGreaterThan(target, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param timeString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> timeGreaterThan(final String timeString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue())).and(timeGreaterThan -> not(stringEmptyOrNull()).test(timeString))
		        .and(timeGreaterThan -> not(stringEmptyOrNull()).test(pattern)).and(timeGreaterThan -> {
			        try {
				        final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern(pattern);
				        final LocalTime timeTest = LocalTime.parse(timeGreaterThan, timeFormat);
				        final LocalTime time = LocalTime.parse(timeString, timeFormat);
				        return timeTest.isAfter(time);
			        } catch (final IllegalArgumentException | DateTimeParseException ex) {
				        return false;
			        }
		        });
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(timeGreaterThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(timeGreaterThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param timeString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> timeGreaterThanOrEqual(final String timeString, final String pattern) {
		return PredicateBuilder.<String>from(timeGreaterThan(timeString, pattern).or(timeEqualTo(timeString, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeLessThan(target.apply(obj), pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeLessThan(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(not(nullValue())).and(obj -> timeLessThan(target, pattern).test(source.apply(obj)));
	}

	/**
	 *
	 * @param timeString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> timeLessThan(final String timeString, final String pattern) {
		return PredicateBuilder.<String>from(not(nullValue())).and(timeLessThan -> not(stringEmptyOrNull()).test(timeString))
		        .and(timeLessThan -> not(stringEmptyOrNull()).test(pattern)).and(timeLessThan -> {
			        try {
				        final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern(pattern);
				        final LocalTime timeTest = LocalTime.parse(timeLessThan, timeFormat);
				        final LocalTime time = LocalTime.parse(timeString, timeFormat);
				        return timeTest.isBefore(time);
			        } catch (final IllegalArgumentException | DateTimeParseException ex) {
				        return false;
			        }
		        });
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern) {
		return PredicateBuilder.<T>from(timeLessThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param <T>
	 * @param source
	 * @param target
	 * @param pattern
	 * @return
	 */
	public static <T> Predicate<T> timeLessThanOrEqual(final Function<T, String> source, final String target, final String pattern) {
		return PredicateBuilder.<T>from(timeLessThan(source, target, pattern).or(timeEqualTo(source, target, pattern)));
	}

	/**
	 *
	 * @param timeString
	 * @param pattern
	 * @return
	 */
	public static Predicate<String> timeLessThanOrEqual(final String timeString, final String pattern) {
		return PredicateBuilder.<String>from(timeLessThan(timeString, pattern).or(timeEqualTo(timeString, pattern)));
	}

}