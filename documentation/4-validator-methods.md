# 4. Validator methods

## 4.1 `failFastRule` when enabled, if one validation group fails the others will not be validated.

* `failFastRule()`

## 4.2 `setPropertyOnContext` when a value instance is being validated it will be available in context with the declared property name

* `setPropertyOnContext(final String property)`

## 4.3 `getPropertyOnContext` when a value instance is available in context with the declared property name it can be retrieved anywhere in the validation tree.

* `getPropertyOnContext(final String property, final Class<P> clazz)`

## 4.4 `validate` instance

* `validate(final T instance)`

## 4.5 `validate` instance with transform results

* `validate(final T instance, final ValidationResultTransform<E> transform)`

## 4.6 `validate` collection

* `validate(final Collection<T> instances)`

## 4.7 `validate` collection with transform results

* `validate(final Collection<T> instances, final ValidationResultTransform<E> transform)`

## 4.8 `ruleFor` instance

* `ruleFor(final Function<T, P> function)`

## 4.9 `ruleForEach` collection

* `ruleForEach(final Function<T, Collection<P>> function)`
