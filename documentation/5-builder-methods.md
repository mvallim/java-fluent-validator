# 5. Chain methods

## 5.1 `critical` path in chain validation

```java
critical()
```

## 5.2 `critical` rise exception path in chain validation

```java
critical(final Class<? extends ValidationException> clazz)
```

## 5.3 `handlerInvalidField` customize generated error to invalid field

```java
handlerInvalidField(final HandlerInvalidField<P> handlerInvalidField)
```

### Examples

* Using full object

    ```java
        ruleFor(entity -> entity)
            .must(stringEquals(Entity::getField1, Entity::getField2))
            .when(not(stringEmptyOrNull(Entity::getProperty01)).and(not(stringEmptyOrNull(Entity::getProperty01))))
            .handlerInvalidField(new HandlerInvalidField<Entity>() {
                @Override
                public Collection<Error> handle(final Entity instance) {
                    final Erro field1 = Error.create("field1", "field1 must be equal field2", "404", instance.getField1());
                    final Erro field2 = Error.create("field2", "field2 must be equal field1", "404", instance.getField2());
                    return Arrays.asList(field1, field2);
                }
            });
    ```

    The result when was error produce two custom errors.

* Using property

    ```java
        ruleFor(Entity::getCollection)
            .must(hasSize(1))
            .when(not(empty()))
            .handlerInvalidField(new HandlerInvalidField<Collection<String>>() {
                @Override
                public Collection<Error> handle(final Collection<String> instance) {
                    return Collections.singletonList(Error.create("collection", "collection must be size 1", "404", instance.getCollection()));
                }
            });
    ```

    The result when was error produce one custom error.

* Using property and access main object

    ```java
        ruleFor(Entity::getCollection)
            .must(hasSize(1))
            .when(not(empty()))
            .handlerInvalidField(new HandlerInvalidField<Collection<String>>() {
                @Override
                public Collection<Error> handle(final Object instance, final Collection<String> value) {
                    final Entity entity = Entity.class.cast(instance);
                    return Collections.singletonList(Error.create("entity", "entity property collection must be size 1", "404", entity));
                }
            });
    ```

    The result when was error produce one custom error.

## 5.4 `must` valid condition

```java
must(final Predicate<P> predicate)
```

## 5.5 `when` condition to validate

```java
when(final Predicate<P> when)
```

## 5.6 `whenever` to validator

```java
whenever(final Predicate<P> predicate)
```

## 5.7 `withAttempedValue` when `must` condition not be true, using object property

```java
withAttempedValue(final Function<T, P> attemptedValue)
```

## 5.8 `withAttempedValue` when `must` condition not be true

```java
withAttempedValue(final P attemptedValue)
```

## 5.9 `withCode` when `must` condition not be true, using object property

```java
withCode(final Function<T, String> code)
```

## 5.10 `withCode` when `must` condition not be true

```java
withCode(final String code)
```

## 5.11 `withFieldName` when `must` condition not be true, using object property

```java
withFieldName(final Function<T, String> fieldName)
```

## 5.12 `withFieldName` when `must` condition not be true

```java
withFieldName(final String fieldName)
```

## 5.13 `withMessage` when `must` condition not be true, using object property

```java
withMessage(final Function<T, String> message)
```

## 5.14 `withMessage` when `must` condition not be true

```java
withMessage(final String message)
```

## 5.15 `withValidator` to validate object

```java
withValidator(final Validator<P> validator)
```
