# 8. PredicateBuilder methods

```java
from(final Predicate<T> predicate)
```

Example:

```java
private static Predicate<Child> isGirl() {
    return PredicateBuilder.from(not(nullValue())).and(instanceOf(Girl.class));
}
```
