# 6. Predicates

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

Represents a predicate (boolean-valued function) of one argument.

## 6.1 Logical

```java
isTrue()
```

```java
isFalse()
```

```java
not(final Predicate<T> predicate)
```

## 6.2 Object

```java
equalTo(final Function<T, Object> source, final Function<T, Object> target)
```

```java
equalTo(final T obj)
```

```java
instanceOf(final Class<?> clazz)
```

```java
instanceOf(final Function<T, Object> source, final Class<?> clazz)
```

```java
nullValue()
```

```java
nullValue(final Function<T, ?> source)
```

## 6.3 String

```java
isAlpha()
```

```java
isAlpha(final Function<T, String> source)
```

```java
isAlphaNumeric()
```

```java
isAlphaNumeric(final Function<T, String> source)
```

```java
isNumber()
```

```java
isNumber(final Function<T, String> source)
```

```java
isNumeric()
```

```java
isNumeric(final Function<T, String> source)
```

```java
stringContains(final Function<T, String> source, final String str)
```

```java
stringContains(final String str)
```

```java
stringEmptyOrNull()
```

```java
stringEmptyOrNull(final Function<T, String> source)
```

```java
stringMatches(final Function<T, String> source, final String regex)
```

```java
stringMatches(final String regex)
```

```java
stringSizeBetween(final Function<T, String> source, final Integer minSize, final Integer maxSize)
```

```java
stringSizeBetween(final Integer minSize, final Integer maxSize)
```

```java
stringSize(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSize(final Function<T, String> source, final Integer size)
```

```java
stringSize(final Integer size)
```

```java
stringSizeGreaterThan(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeGreaterThan(final Function<T, String> source, final Integer size)
```

```java
stringSizeGreaterThan(final Integer size)
```

```java
stringSizeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeGreaterThanOrEqual(final Function<T, String> source, final Integer size)
```

```java
stringSizeGreaterThanOrEqual(final Integer size)
```

```java
stringSizeLessThan(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeLessThan(final Function<T, String> source, final Integer size)
```

```java
stringSizeLessThan(final Integer size)
```

```java
stringSizeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeLessThanOrEqual(final Function<T, String> source, final Integer size)
```

```java
stringSizeLessThanOrEqual(final Integer size)
```

## 6.4 Comparable

```java
between(final E min, final E max)
```

```java
between(final Function<T, E> source, final E min, final E max)
```

```java
equalTo(final E value)
```

```java
equalTo(final Function<T, E> source, final E value)
```

```java
equalTo(final Function<T, E> source, final Function<T, E> target)
```

```java
greaterThan(final E min)
```

```java
greaterThan(final Function<T, E> source, final E min)
```

```java
greaterThan(final Function<T, E> source, final Function<T, E> sourceMin)
```

```java
greaterThanOrEqual(final E min)
```

```java
greaterThanOrEqual(final Function<T, E> source, final E min)
```

```java
greaterThanOrEqual(final Function<T, E> source, final Function<T, E> sourceMin)
```

```java
lessThan(final E max)
```

```java
lessThan(final Function<T, E> source, final E max)
```

```java
lessThan(final Function<T, E> source, final Function<T, E> sourceMax)
```

```java
lessThanOrEqual(final E max)
```

```java
lessThanOrEqual(final Function<T, E> source, final E max)
```

```java
lessThanOrEqual(final Function<T, E> source, final Function<T, E> sourceMax)
```

## 6.5 Collection

```java
empty()
```

```java
empty(final Function<T, Collection<E>> source)
```

```java
hasAny(final Collection<E> objects)
```

```java
hasAny(final E[] objects)
```

```java
hasAny(final Function<T, Collection<E>> source, final Collection<E> objects)
```

```java
hasAny(final Function<T, Collection<E>> source, final E[] objects)
```

```java
hasItem(final E object)
```

```java
hasItem(final Function<T, Collection<E>> source, final E object)
```

```java
hasItems(final Collection<E> objects)
```

```java
hasItems(final E[] objects)
```

```java
hasItems(final Function<T, Collection<E>> source, final Collection<E> objects)
```

```java
hasItems(final Function<T, Collection<E>> source, final E[] objects)
```

```java
hasSize(final Function<T, Collection<E>> source, final Integer size)
```

```java
hasSize(final Integer size)
```

## 6.6 Date

```java
dateBetween(final Function<T, String> source, final String dateStringMin, final String dateStringMax, final String pattern)
```

```java
dateBetween(final String dateStringMin, final String dateStringMax, final String pattern)
```

```java
dateEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern) {
```

```java
dateEqualTo(final Function<T, String> source, final String target, final String pattern)
```

```java
dateEqualTo(final String dateString, final String pattern)
```

```java
dateGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern) {
```

```java
dateGreaterThan(final Function<T, String> source, final String target, final String pattern)
```

```java
dateGreaterThan(final String dateString, final String pattern)
```

```java
dateGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern)
```

```java
dateGreaterThanOrEqual(final String dateString, final String pattern)
```

```java
dateLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateLessThan(final Function<T, String> source, final String target, final String pattern)
```

```java
dateLessThan(final String dateString, final String pattern)
```

```java
dateLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateLessThanOrEqual(final Function<T, String> source, final String target, final String pattern)
```

```java
dateLessThanOrEqual(final String dateString, final String pattern)
```

## 6.7 DateTime

```java
dateTimeBetween(final Function<T, String> source, final String dateStringMin, final String dateStringMax, final String pattern)
```

```java
dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern)
```

```java
dateTimeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeEqualTo(final Function<T, String> source, final String target, final String pattern)
```

```java
dateTimeEqualTo(final String dateString, final String pattern)
```

```java
dateTimeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeGreaterThan(final Function<T, String> source, final String target, final String pattern)
```

```java
dateTimeGreaterThan(final String dateString, final String pattern)
```

```java
dateTimeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern)
```

```java
dateTimeGreaterThanOrEqual(final String dateString, final String pattern)
```

```java
dateTimeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeLessThan(final Function<T, String> source, final String target, final String pattern)
```

```java
dateTimeLessThan(final String dateString, final String pattern)
```

```java
dateTimeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeLessThanOrEqual(final Function<T, String> source, final String target, final String pattern)
```

```java
dateTimeLessThanOrEqual(final String dateString, final String pattern)
```

## 6.8 Time

```java
timeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
timeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
timeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
timeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
timeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
timeEqualTo(final Function<T, String> source, final String target, final String pattern)
```

```java
timeGreaterThan(final Function<T, String> source, final String target, final String pattern)
```

```java
timeLessThan(final Function<T, String> source, final String target, final String pattern)
```

```java
timeGreaterThanOrEqual(final Function<T, String> source, final String target, final String pattern)
```

```java
timeLessThanOrEqual(final Function<T, String> source, final String target, final String pattern)
```

```java
timeBetween(final Function<T, String> source, final String timeStringMin, final String timeStringMax, final String pattern)
```

```java
timeEqualTo(final String timeString, final String pattern)
```

```java
timeGreaterThan(final String timeString, final String pattern)
```

```java
timeLessThan(final String timeString, final String pattern)
```

```java
timeGreaterThanOrEqual(final String timeString, final String pattern)
```

```java
timeLessThanOrEqual(final String timeString, final String pattern)
```

```java
timeBetween(final String timeStringMin, final String timeStringMax, final String pattern)
```
