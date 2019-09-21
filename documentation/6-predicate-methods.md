# 6. Predicates

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

Represents a predicate (boolean-valued function) of one argument.

## 6.1 Logical

```java
not(final Predicate<T> predicate)
```

```java
isTrue()
```

```java
isFalse()
```

## 6.2 Object

```java
nullValue()
```

```java
equalTo(final T obj)
```

```java
instanceOf(final Class<?> clazz)
```

## 6.3 String

```java
stringSize(final Integer size)
```

```java
stringSizeGreaterThan(final Integer size)
```

```java
stringSizeLessThan(final int size)
```

```java
stringSizeGreaterThanOrEqual(final Integer size)
```

```java
stringSizeLessThanOrEqual(final Integer size)
```

```java
stringSizeBetween(final Integer minSize, final Integer maxSize)
```

```java
stringEmptyOrNull()
```

```java
stringContains(final String str)
```

```java
stringMatches(final String regex)
```

```java
isNumeric()
```

```java
isAlpha()
```

```java
isAlphaNumeric()
```

```java
isNumber()
```

## 6.4 Comparable

```java
lessThan(final T max)
```

```java
greaterThan(final T min)
```

```java
greaterThanOrEqual(final T min)
```

```java
lessThanOrEqual(final T max)
```

```java
between(final T min, final T max)

## 6.5 Collection

```java
empty()
```

```java
hasItem(final Object object)
```

```java
hasItems(final Collection<Object> objects)
```

```java
hasItems(final Object... objects)
```

```java
hasAny(final Collection<Object> objects)
```

```java
hasAny(final Object... objects)
```

```java
hasSize(final Integer size)
```

## 6.6 DateTime

```java
dateTimeEqualTo(final String dateString, final String pattern)
```

```java
dateTimeGreaterThan(final String dateString, final String pattern)
```

```java
dateTimeLessThan(final String dateString, final String pattern)
```

```java
dateTimeGreaterThanOrEqual(final String dateString, final String pattern)
```

```java
dateTimeLessThanOrEqual(final String dateString, final String pattern)
```

```java
dateTimeBetween(final String dateStringMin, final String dateStringMax, final String pattern)
```

## 6.7 Predicate inter properties

### 6.7.1 Comparable

```java
equalTo(final Function<T, E> source, final Function<T, E> target)
```

```java
lessThan(final Function<T, E> source, final Function<T, E> target)
```

```java
lessThanOrEqual(final Function<T, E> source, final Function<T, E> target)
```

```java
greaterThan(final Function<T, E> source, final Function<T, E> target)
```

```java
greaterThanOrEqual(final Function<T, E> source, final Function<T, E> target)
```

### 6.7.2 String

```java
stringSize(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeGreaterThan(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeLessThan(final Function<T, String> source, final Function<T, String> target)
```

```java
stringSizeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target)
```

### 6.7.2 DateTime

```java
dateTimeEqualTo(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeGreaterThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeLessThan(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeGreaterThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```

```java
dateTimeLessThanOrEqual(final Function<T, String> source, final Function<T, String> target, final String pattern)
```
