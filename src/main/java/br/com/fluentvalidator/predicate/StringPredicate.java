/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
import static br.com.fluentvalidator.predicate.LogicalPredicate.is;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalObject;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;
import java.util.Collection;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

public final class StringPredicate {

  /**
   * Returns a predicate that checks if a string contains only alphabetic characters.
   * The string must not be null or empty.
   *
   * @return a predicate that tests if a string is composed exclusively of letters
   */
  public static Predicate<String> isAlpha() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull()))
        .and(isNumeric -> isNumeric.chars().allMatch(Character::isLetter));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it contains only alphabetic characters.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @return a predicate that tests if the extracted string is composed exclusively of letters
   */
  public static <T> Predicate<T> isAlpha(final Function<T, String> source) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isAlpha().test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string contains only alphanumeric characters
   * (letters and digits). The string must not be null or empty.
   *
   * @return a predicate that tests if a string is composed exclusively of letters or digits
   */
  public static Predicate<String> isAlphaNumeric() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull()))
        .and(isNumeric -> isNumeric.chars().allMatch(Character::isLetterOrDigit));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it contains only alphanumeric characters.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @return a predicate that tests if the extracted string is alphanumeric
   */
  public static <T> Predicate<T> isAlphaNumeric(final Function<T, String> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> isAlphaNumeric().test(source.apply(obj)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it represents a valid date according to the specified pattern.
   * Uses strict resolver style for date parsing.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param pattern the date pattern to validate against
   * @return a predicate that tests if the extracted string is a valid date
   */
  public static <T> Predicate<T> isDate(final Function<T, String> source, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isDate(pattern).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string represents a valid date
   * according to the specified pattern. Uses strict resolver style for date parsing.
   *
   * @param pattern the date pattern to validate against
   * @return a predicate that tests if a string is a valid date
   */
  public static Predicate<String> isDate(final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(isDate -> not(stringEmptyOrNull()).test(pattern))
        .and(isDate -> {
          try {
            final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern).withResolverStyle(ResolverStyle.STRICT);
            return Objects.nonNull(LocalDate.parse(isDate, dateFormat));
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it represents a valid date-time according to the specified pattern.
   * Uses strict resolver style for date-time parsing.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param pattern the date-time pattern to validate against
   * @return a predicate that tests if the extracted string is a valid date-time
   */
  public static <T> Predicate<T> isDateTime(final Function<T, String> source, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isDateTime(pattern).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string represents a valid date-time
   * according to the specified pattern. Uses strict resolver style for date-time parsing.
   *
   * @param pattern the date-time pattern to validate against
   * @return a predicate that tests if a string is a valid date-time
   */
  public static Predicate<String> isDateTime(final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(isDateTime -> not(stringEmptyOrNull()).test(pattern))
        .and(isDateTime -> {
          try {
            final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern).withResolverStyle(ResolverStyle.STRICT);
            return Objects.nonNull(LocalDateTime.parse(isDateTime, dateFormat));
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   * Returns a predicate that checks if a string represents a valid number.
   * The string must not be null or empty. Uses BigDecimal for parsing.
   *
   * @return a predicate that tests if a string is a valid number
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

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it represents a valid number.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @return a predicate that tests if the extracted string is a valid number
   */
  public static <T> Predicate<T> isNumber(final Function<T, String> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> isNumber().test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string contains only numeric digits.
   * The string must not be null or empty.
   *
   * @return a predicate that tests if a string is composed exclusively of digits
   */
  public static Predicate<String> isNumeric() {
    return PredicateBuilder.<String>from(not(stringEmptyOrNull()))
        .and(isNumeric -> isNumeric.chars().allMatch(Character::isDigit));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it contains only numeric digits.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @return a predicate that tests if the extracted string is composed exclusively of digits
   */
  public static <T> Predicate<T> isNumeric(final Function<T, String> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> isNumeric().test(source.apply(obj)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it represents a valid time according to the specified pattern.
   * Uses strict resolver style for time parsing.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param pattern the time pattern to validate against
   * @return a predicate that tests if the extracted string is a valid time
   */
  public static <T> Predicate<T> isTime(final Function<T, String> source, final String pattern) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> isTime(pattern).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string represents a valid time
   * according to the specified pattern. Uses strict resolver style for time parsing.
   *
   * @param pattern the time pattern to validate against
   * @return a predicate that tests if a string is a valid time
   */
  public static Predicate<String> isTime(final String pattern) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(isTime -> not(stringEmptyOrNull()).test(pattern))
        .and(isTime -> {
          try {
            final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(pattern).withResolverStyle(ResolverStyle.STRICT);
            return Objects.nonNull(LocalTime.parse(isTime, dateFormat));
          } catch (final IllegalArgumentException | DateTimeParseException ex) {
            return false;
          }
        });
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it contains the specified substring.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param str the substring to search for
   * @return a predicate that tests if the extracted string contains the specified substring
   */
  public static <T> Predicate<T> stringContains(final Function<T, String> source, final String str) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringContains(str).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string contains the specified substring.
   * Both the string and the substring must not be null.
   *
   * @param str the substring to search for
   * @return a predicate that tests if a string contains the specified substring
   */
  public static Predicate<String> stringContains(final String str) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringContains -> not(nullValue()).test(str))
        .and(stringContains -> stringContains.contains(str));
  }

  /**
   * Returns a predicate that checks if a string is empty or null.
   *
   * @return a predicate that tests if a string is null or empty
   */
  public static Predicate<String> stringEmptyOrNull() {
    return PredicateBuilder.<String>from(is(nullValue())).or(String::isEmpty);
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it is empty or null.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @return a predicate that tests if the extracted string is empty or null
   */
  public static <T> Predicate<T> stringEmptyOrNull(final Function<T, String> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringEmptyOrNull().test(source.apply(obj)));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if they are equal.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare against
   * @return a predicate that tests if the extracted strings are equal
   */
  public static <T> Predicate<T> stringEquals(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringEquals(source, target.apply(obj)).test(obj));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it equals the specified value.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param value the expected string value
   * @return a predicate that tests if the extracted string equals the specified value
   */
  public static <T> Predicate<T> stringEquals(final Function<T, String> source, final String value) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(source.apply(obj)))
        .and(obj -> stringEquals(value).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string equals the specified value.
   *
   * @param <T> the type of the input object
   * @param value the expected string value
   * @return a predicate that tests if a string equals the specified value
   */
  public static <T> Predicate<T> stringEquals(final String value) {
    return PredicateBuilder.<T>from(not(nullValue())).and(obj -> obj.equals(value));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if they are equal, ignoring case differences.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare against
   * @return a predicate that tests if the extracted strings are equal ignoring case
   */
  public static <T> Predicate<T> stringEqualsIgnoreCase(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringEqualsIgnoreCase(source, target.apply(obj)).test(obj));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it equals the specified value, ignoring case differences.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param value the expected string value
   * @return a predicate that tests if the extracted string equals the specified value ignoring case
   */
  public static <T> Predicate<T> stringEqualsIgnoreCase(final Function<T, String> source, final String value) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(source.apply(obj)))
        .and(obj -> stringEqualsIgnoreCase(value).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string equals the specified value,
   * ignoring case differences. Both the string and the value must not be null.
   *
   * @param value the expected string value
   * @return a predicate that tests if a string equals the specified value ignoring case
   */
  public static Predicate<String> stringEqualsIgnoreCase(final String value) {
    return PredicateBuilder.<String>from(not(nullValue())).and(obj -> not(nullValue()).test(value))
        .and(obj -> obj.equalsIgnoreCase(value));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it matches the specified regular expression.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param regex the regular expression to match against
   * @return a predicate that tests if the extracted string matches the regex
   */
  public static <T> Predicate<T> stringMatches(final Function<T, String> source, final String regex) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringMatches(regex).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string matches the specified regular expression.
   * Both the string and the regex must not be null.
   *
   * @param regex the regular expression to match against
   * @return a predicate that tests if a string matches the regex
   */
  public static Predicate<String> stringMatches(final String regex) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringMatches -> not(nullValue()).test(regex))
        .and(stringMatches -> stringMatches.matches(regex));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if they have the same length.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare length against
   * @return a predicate that tests if the extracted strings have equal length
   */
  public static <T> Predicate<T> stringSize(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSize(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if its length equals the specified size.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param size the expected string length
   * @return a predicate that tests if the extracted string has the specified length
   */
  public static <T> Predicate<T> stringSize(final Function<T, String> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringSize(size).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string has the specified length.
   * Both the string and the size must not be null.
   *
   * @param size the expected string length
   * @return a predicate that tests if a string has the specified length
   */
  public static Predicate<String> stringSize(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSize -> not(nullValue()).test(size))
        .and(stringSize -> equalObject(size).test(stringSize.length()));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if its length is between the specified minimum and maximum (inclusive).
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param minSize the minimum acceptable string length
   * @param maxSize the maximum acceptable string length
   * @return a predicate that tests if the extracted string length is within the specified range
   */
  public static <T> Predicate<T> stringSizeBetween(final Function<T, String> source, final Integer minSize, final Integer maxSize) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringSizeBetween(minSize, maxSize).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string length is between the specified
   * minimum and maximum (inclusive). The string must not be null.
   *
   * @param minSize the minimum acceptable string length
   * @param maxSize the maximum acceptable string length
   * @return a predicate that tests if a string length is within the specified range
   */
  public static Predicate<String> stringSizeBetween(final Integer minSize, final Integer maxSize) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeGreaterThanOrEqual(minSize).and(stringSizeLessThanOrEqual(maxSize)));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if the source string length is greater than the target string length.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare length against
   * @return a predicate that tests if the source string is longer than the target string
   */
  public static <T> Predicate<T> stringSizeGreaterThan(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSizeGreaterThan(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if its length is greater than the specified size.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param size the minimum length threshold (exclusive)
   * @return a predicate that tests if the extracted string length is greater than the specified size
   */
  public static <T> Predicate<T> stringSizeGreaterThan(final Function<T, String> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringSizeGreaterThan(size).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string length is greater than the specified size.
   * Both the string and the size must not be null.
   *
   * @param size the minimum length threshold (exclusive)
   * @return a predicate that tests if a string length is greater than the specified size
   */
  public static Predicate<String> stringSizeGreaterThan(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeGreaterThan -> not(nullValue()).test(size))
        .and(stringSizeGreaterThan -> greaterThan(size).test(stringSizeGreaterThan.length()));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if the source string length is greater than or equal to the target string length.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare length against
   * @return a predicate that tests if the source string length is greater than or equal to the target
   */
  public static <T> Predicate<T> stringSizeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(stringSizeGreaterThan(source, target).or(stringSize(source, target)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if its length is greater than or equal to the specified size.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param size the minimum length threshold (inclusive)
   * @return a predicate that tests if the extracted string length is greater than or equal to the specified size
   */
  public static <T> Predicate<T> stringSizeGreaterThanOrEqual(final Function<T, String> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(stringSizeGreaterThan(source, size).or(stringSize(source, size)));
  }

  /**
   * Returns a predicate that checks if a string length is greater than or equal to the specified size.
   * Both the string and the size must not be null.
   *
   * @param size the minimum length threshold (inclusive)
   * @return a predicate that tests if a string length is greater than or equal to the specified size
   */
  public static Predicate<String> stringSizeGreaterThanOrEqual(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeGreaterThan(size).or(stringSize(size)));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if the source string length is less than the target string length.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare length against
   * @return a predicate that tests if the source string is shorter than the target string
   */
  public static <T> Predicate<T> stringSizeLessThan(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(target.apply(obj)))
        .and(obj -> stringSizeLessThan(target.apply(obj).length()).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if its length is less than the specified size.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param size the maximum length threshold (exclusive)
   * @return a predicate that tests if the extracted string length is less than the specified size
   */
  public static <T> Predicate<T> stringSizeLessThan(final Function<T, String> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> stringSizeLessThan(size).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if a string length is less than the specified size.
   * Both the string and the size must not be null.
   *
   * @param size the maximum length threshold (exclusive)
   * @return a predicate that tests if a string length is less than the specified size
   */
  public static Predicate<String> stringSizeLessThan(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeLessThan -> not(nullValue()).test(size))
        .and(stringSizeLessThan -> lessThan(size).test(stringSizeLessThan.length()));
  }

  /**
   * Returns a predicate that extracts two strings from the given source and target functions
   * and checks if the source string length is less than or equal to the target string length.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the source string
   * @param target a function that extracts the target string to compare length against
   * @return a predicate that tests if the source string length is less than or equal to the target
   */
  public static <T> Predicate<T> stringSizeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(stringSizeLessThan(source, target).or(stringSize(source, target)));
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if its length is less than or equal to the specified size.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param size the maximum length threshold (inclusive)
   * @return a predicate that tests if the extracted string length is less than or equal to the specified size
   */
  public static <T> Predicate<T> stringSizeLessThanOrEqual(final Function<T, String> source, final Integer size) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(stringSizeLessThan(source, size).or(stringSize(source, size)));
  }

  /**
   * Returns a predicate that checks if a string length is less than or equal to the specified size.
   * Both the string and the size must not be null.
   *
   * @param size the maximum length threshold (inclusive)
   * @return a predicate that tests if a string length is less than or equal to the specified size
   */
  public static Predicate<String> stringSizeLessThanOrEqual(final Integer size) {
    return PredicateBuilder.<String>from(not(nullValue()))
        .and(stringSizeLessThan(size).or(stringSize(size)));
  }

  /**
   * Returns a predicate that checks if a string is contained in the specified collection.
   * Both the string and the collection must not be null.
   *
   * @param <T> the type of elements in the collection, must extend String
   * @param collection the collection to search in
   * @return a predicate that tests if a string is present in the specified collection
   */
  public static <T extends String> Predicate<T> stringInCollection(final Collection<String> collection) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> not(nullValue()).test(collection))
        .and(collection::contains);
  }

  /**
   * Returns a predicate that extracts a string from the given source function
   * and checks if it is contained in the specified collection.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param collection the collection to search in
   * @return a predicate that tests if the extracted string is present in the specified collection
   */
  public static <T> Predicate<T> stringInCollection(final Function<T, String> source, final Collection<String> collection) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(obj -> not(nullValue()).test(collection))
        .and(obj -> stringInCollection(collection).test(source.apply(obj)));
  }

  /**
   * Returns a predicate that checks if the specified string is contained
   * in a collection extracted from the given target function.
   *
   * @param <T> the type of the input object
   * @param source the string to search for
   * @param target a function that extracts the collection to search in
   * @return a predicate that tests if the string is present in the extracted collection
   */
  public static <T> Predicate<T> stringInCollection(final String source, final Function<T, Collection<String>> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(target)))
        .and(obj -> stringInCollection(target.apply(obj)).test(source));
  }

  /**
   * Returns a predicate that extracts a string and a collection from the given
   * source and target functions, and checks if the string is contained in the collection.
   *
   * @param <T> the type of the input object
   * @param source a function that extracts the string to be validated
   * @param target a function that extracts the collection to search in
   * @return a predicate that tests if the extracted string is present in the extracted collection
   */
  public static <T> Predicate<T> stringInCollection(final Function<T, String> source, final Function<T, Collection<String>> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(source)))
        .and(not(nullValue(target)))
        .and(obj -> stringInCollection(target.apply(obj)).test(source.apply(obj)));
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private StringPredicate() {
    super();
  }

}
