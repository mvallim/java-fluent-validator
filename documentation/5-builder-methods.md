# 5. Chain methods

## 5.1 `when` condition to validate

* `when(final Predicate<T> when)`

## 5.2 `must` condition to valid

* `must(final Predicate<T> must)`

## 5.3 `withMessage` when `must` condition not be true

* `withMessage(final String message)`

## 5.4 `withCode` when `must` condition not be true

* `withCode(final String code)`

## 5.5 `withFieldName` when `must` condition not be true

* `withFieldName(final String fieldName)`

## 5.6 `whenever` to validator

* `whenever(final Predicate<T> whenever)`

## 5.7 `withValidator` to validate object

* `withValidator(final Validator<P> validator)`

## 5.8 `critical` path in chain validation

* `critical()`

## 5.9 `critical` rise exception path in chain validation

* `critical(final Class<? extends ValidationException> clazz)`
