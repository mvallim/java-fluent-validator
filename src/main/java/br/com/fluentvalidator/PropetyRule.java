package br.com.fluentvalidator;

import java.util.function.Function;
import java.util.function.Predicate;

class PropetyRule<T, P> implements WhenProperty<P>, Rule<T> {

	private final Function<T, P> propertyFunction;

	private Predicate<P> when;

	private ValidationBuilder<P> validation;

	public PropetyRule(final Function<T, P> propertyFunction) {
		this.propertyFunction = propertyFunction;
	}

	@Override
	public ValidationBuilder<P> when(final Predicate<P> predicate) {
		this.when = predicate;
		this.validation = new ValidationRuleBuilder<>();
		return this.validation;
	}

	@Override
	public void apply(final T instance) {
		final P value = this.propertyFunction.apply(instance);
		if (this.when.test(value)) {
			this.validation.apply(value);
		}
	}

}
