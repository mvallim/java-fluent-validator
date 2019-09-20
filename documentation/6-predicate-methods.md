# 6. Predicates

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

Represents a predicate (boolean-valued function) of one argument.

## 6.1 Logical

* `not(final Predicate<T> predicate)`
* `isTrue()`
* `isFalse()`

## 6.2 Object

* `nullValue()`
* `equalTo(final T obj)`
* `instanceOf(final Class<?> clazz)`

## 6.3 String

* `stringSize(final int size)`
* `stringSizeGreaterThan(final int size)`
* `stringSizeLessThan(final int size)`
* `stringSizeGreaterThanOrEqual(final int size)`
* `stringSizeLessThanOrEqual(final int size)`
* `stringSizeBetween(final int minSize, final int maxSize)`
* `stringEmptyOrNull()`
* `stringContains(final String str)`
* `stringMatches(final String regex)`
* `isNumeric()`
* `isAlpha()`
* `isAlphaNumeric()`
* `isNumber()`

## 6.4 Comparable

* `lessThan(final T max)`
* `greaterThan(final T min)`
* `greaterThanOrEqual(final T min)`
* `lessThanOrEqual(final T max)`
* `between(final T min, final T max)`

## 6.5 Collection

* `empty()`
* `hasItem(final Object object)`
* `hasItems(final Collection<Object> objects)`
* `hasItems(final Object... objects)`
* `hasAny(final Collection<Object> objects)`
* `hasAny(final Object... objects)`
* `hasSize(final int size)`

## 6.6 DateTime

* `dateTimeEqualTo(final String dateString, final String pattern)`
* `dateTimeGreaterThan(final String dateString, final String pattern)`
* `dateTimeLessThan(final String dateString, final String pattern)`
* `dateTimeGreaterThanOrEqual(final String dateString, final String pattern)`
* `dateTimeLessThanOrEqual(final String dateString, final String pattern)`
* `dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern)`
* `dateTimeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)`

