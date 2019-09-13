# 4. Validator methods

## 4.1 `validate` instance

* `validate(final T instance)`

## 4.2 `validate` instance with transform results

* `validate(final T instance, final ValidationResultTransform<E> transform)`

## 4.3 `validate` collection

* `validate(final Collection<T> instances)`

## 4.4 `validate` collection with transform results

* `validate(final Collection<T> instances, final ValidationResultTransform<E> transform)`

## 4.5 `ruleFor` instance

* `ruleFor(final Function<T, P> function)`

## 4.6 `ruleForEach` collection

* `ruleForEach(final Function<T, Collection<P>> function)`
