package br.com.fluentvalidator.builder;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.ValidationResult;

public interface Validator<T> extends Rule<T> {

	ValidationResult validate(final T instance);

	List<ValidationResult> validate(final Collection<T> instances);

	<P> RuleProperty<T, P> ruleFor(final Function<T, P> function);

	<P> RuleCollection<T, P> ruleForEach(final Function<T, Collection<P>> function);

}
