package br.com.fluentvalidator;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

public interface Validator<T> {

	Collection<Rule<T>> getRules();

	ValidationResult validate(final T instance);

	List<ValidationResult> validate(final Collection<T> instances);

	<P> WhenProperty<P> ruleFor(final Function<T, P> function);

	<P> WhenCollection<P> ruleForEach(final Function<T, Collection<P>> function);

}
