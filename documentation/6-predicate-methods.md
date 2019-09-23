# 6. Predicates

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

Represents a predicate (boolean-valued function) of one argument.

## 6.1 Logical

```java
not(Predicate<T> predicate)
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
equalTo(T obj)
```

```java
instanceOf(Class<?> clazz)
```

## 6.3 String

```java
stringSize(Integer size)
```

```java
stringSizeGreaterThan(Integer size)
```

```java
stringSizeLessThan(Integer size)
```

```java
stringSizeGreaterThanOrEqual(Integer size)
```

```java
stringSizeLessThanOrEqual(Integer size)
```

```java
stringSizeBetween(Integer minSize, Integer maxSize)
```

```java
stringEmptyOrNull()
```

```java
stringContains(String str)
```

```java
stringMatches(String regex)
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
lessThan(T max)
```

```java
greaterThan(T min)
```

```java
greaterThanOrEqual(T min)
```

```java
lessThanOrEqual(T max)
```

```java
between(T min, T max)
```

## 6.5 Collection

```java
empty()
```

```java
hasItem(Object object)
```

```java
hasItems(Collection<Object> objects)
```

```java
hasItems(Object... objects)
```

```java
hasAny(Collection<Object> objects)
```

```java
hasAny(Object... objects)
```

```java
hasSize(Integer size)
```

## 6.6 DateTime

```java
dateTimeEqualTo(String dateString, String pattern)
```

```java
dateTimeGreaterThan(String dateString, String pattern)
```

```java
dateTimeLessThan(String dateString, String pattern)
```

```java
dateTimeGreaterThanOrEqual(String dateString, String pattern)
```

```java
dateTimeLessThanOrEqual(String dateString, String pattern)
```

```java
dateTimeBetween(String dateStringMin, String dateStringMax, String pattern)
```

## 6.7 Predicate inter properties

### 6.7.1 Comparable

```java
equalTo(Function<T, E> source, Function<T, E> target)
```

```java
lessThan(Function<T, E> source, Function<T, E> target)
```

```java
lessThanOrEqual(Function<T, E> source, Function<T, E> target)
```

```java
greaterThan(Function<T, E> source, Function<T, E> target)
```

```java
greaterThanOrEqual(Function<T, E> source, Function<T, E> target)
```

### 6.7.2 String

```java
stringSize(Function<T, String> source, Function<T, String> target)
```

```java
stringSizeGreaterThan(Function<T, String> source, Function<T, String> target)
```

```java
stringSizeGreaterThanOrEqual(Function<T, String> source, Function<T, String> target)
```

```java
stringSizeLessThan(Function<T, String> source, Function<T, String> target)
```

```java
stringSizeLessThanOrEqual(Function<T, String> source, Function<T, String> target)
```

### 6.7.2 DateTime

```java
dateTimeEqualTo(Function<T, String> source, Function<T, String> target, String pattern)
```

```java
dateTimeGreaterThan(Function<T, String> source, Function<T, String> target, String pattern)
```

```java
dateTimeLessThan(Function<T, String> source, Function<T, String> target, String pattern)
```

```java
dateTimeGreaterThanOrEqual(Function<T, String> source, Function<T, String> target, String pattern)
```

```java
dateTimeLessThanOrEqual(Function<T, String> source, Function<T, String> target, String pattern)
```
