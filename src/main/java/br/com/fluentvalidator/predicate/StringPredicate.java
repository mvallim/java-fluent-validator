package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.math.BigDecimal;
import java.util.function.Function;
import java.util.function.Predicate;

public final class StringPredicate {

  private StringPredicate() {
    super();
  }

  /**
   *
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> stringSize(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSize(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> stringSizeGreaterThan(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSizeGreaterThan(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> stringSizeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSizeGreaterThanOrEqual(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> stringSizeLessThan(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSizeLessThan(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @return
   */
  public static <T> Predicate<T> stringSizeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSizeLessThanOrEqual(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   *
   * @param size
   * @return
   */
  public static Predicate<String> stringSize(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSize -> not(nullValue()).test(size))
        .and(stringSize -> equalTo(size).test(stringSize.length()));
  }

  /**
   *
   * @param size
   * @return
   */
  public static Predicate<String> stringSizeGreaterThan(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeGreaterThan -> not(nullValue()).test(size))
        .and(stringSizeGreaterThan -> greaterThan(size).test(stringSizeGreaterThan.length()));
  }

  /**
   *
   * @param size
   * @return
   */
  public static Predicate<String> stringSizeLessThan(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeLessThan -> not(nullValue()).test(size))
        .and(stringSizeLessThan -> lessThan(size).test(stringSizeLessThan.length()));
  }

  /**
   *
   * @param size
   * @return
   */
  public static Predicate<String> stringSizeGreaterThanOrEqual(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeGreaterThanOrEqual -> not(nullValue()).test(size))
        .and(stringSizeGreaterThanOrEqual -> greaterThanOrEqual(size).test(stringSizeGreaterThanOrEqual.length()));
  }

  /**
   *
   * @param size
   * @return
   */
  public static Predicate<String> stringSizeLessThanOrEqual(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeLessThanOrEqual -> not(nullValue()).test(size))
        .and(stringSizeLessThanOrEqual -> lessThanOrEqual(size).test(stringSizeLessThanOrEqual.length()));
  }

  /**
   *
   * @param minSize
   * @param maxSize
   * @return
   */
  public static Predicate<String> stringSizeBetween(final Integer minSize, final Integer maxSize) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeGreaterThanOrEqual(minSize).and(stringSizeLessThanOrEqual(maxSize)));
  }

  /**
   *
   * @return
   */
  public static Predicate<String> stringEmptyOrNull() {
    return PredicateBuilder.<String>from(is(nullValue()))
        .or(String::isEmpty);
  }

  /**
   *
   * @param str
   * @return
   */
  public static Predicate<String> stringContains(final String str) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringContains -> not(nullValue()).test(str))
        .and(stringContains -> stringContains.contains(str));
  }

  /**
   *
   * @param regex
   * @return
   */
  public static Predicate<String> stringMatches(final String regex) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringMatches -> not(nullValue()).test(regex))
        .and(stringMatches -> stringMatches.matches(regex));
  }

  /**
   *
   * @return
   */
  public static Predicate<String> isNumeric() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull()))
        .and(isNumeric -> isNumeric.chars().allMatch(Character::isDigit));
  }

  /**
   *
   * @return
   */
  public static Predicate<String> isAlpha() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull()))
        .and(isNumeric -> isNumeric.chars().allMatch(Character::isLetter));
  }

  /**
   *
   * @return
   */
  public static Predicate<String> isAlphaNumeric() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull()))
        .and(isNumeric -> isNumeric.chars().allMatch(Character::isLetterOrDigit));
  }

  /**
   *
   * @return
   */
  public static Predicate<String> isNumber() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull())).and(isNumber -> {
      try {
        new BigDecimal(isNumber);
      } catch (final NumberFormatException e) {
        return false;
      }
      return true;
    });
  }

}
