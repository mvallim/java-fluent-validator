package br.com.fluentvalidator.builder;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.ValidationResult;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public interface Validator<T> extends Rule<T> {

	ValidationResult validate(final T instance);

	<E> E validate(final T instance, final ValidationResultTransform<E> transform);

	List<ValidationResult> validate(final Collection<T> instances);

	<E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> transform);

	<P> RuleProperty<T, P> ruleFor(final Function<T, P> function);

	<P> RuleCollection<T, P> ruleForEach(final Function<T, Collection<P>> function);

}
