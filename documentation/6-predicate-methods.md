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

* `stringSize(final Integer size)`
* `stringSizeGreaterThan(final Integer size)`
* `stringSizeLessThan(final int size)`
* `stringSizeGreaterThanOrEqual(final Integer size)`
* `stringSizeLessThanOrEqual(final Integer size)`
* `stringSizeBetween(final Integer minSize, final Integer maxSize)`
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
* `hasSize(final Integer size)`

## 6.6 DateTime

* `dateTimeEqualTo(final String dateString, final String pattern)`
* `dateTimeGreaterThan(final String dateString, final String pattern)`
* `dateTimeLessThan(final String dateString, final String pattern)`
* `dateTimeGreaterThanOrEqual(final String dateString, final String pattern)`
* `dateTimeLessThanOrEqual(final String dateString, final String pattern)`
* `dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern)`

## 6.7 Predicate inter properties

### 6.7.1 Comparable

* `equalTo(final Function<T, E> source, final Function<T, E> target)`
* `lessThan(final Function<T, E> source, final Function<T, E> target)`
* `lessThanOrEqual(final Function<T, E> source, final Function<T, E> target)`
* `greaterThan(final Function<T, E> source, final Function<T, E> target)`
* `greaterThanOrEqual(final Function<T, E> source, final Function<T, E> target)`

### 6.7.2 String

* `stringSize(final Function<T, String> source, final Function<T, String> target)`
* `stringSizeGreaterThan(final Function<T, String> source, final Function<T, String> target)`
* `stringSizeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target)`
* `stringSizeLessThan(final Function<T, String> source, final Function<T, String> target)`
* `stringSizeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target)`

### 6.7.2 DateTime

* `dateTimeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)`
* `dateTimeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)`

