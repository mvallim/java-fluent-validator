package br.com.fluentvalidator;

import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

class CollectionPropetyRule<T, P> implements WhenCollection<P>, Rule<T> {

	protected final Function<T, Collection<P>> collectionFunction;

	protected Predicate<Collection<P>> when;

	protected ValidationBuilder<P> validation;

	public CollectionPropetyRule(final Function<T, Collection<P>> collectionFunction) {
		this.collectionFunction = collectionFunction;
	}

	@Override
	public ValidationBuilder<P> when(final Predicate<Collection<P>> predicate) {
		this.when = predicate;
		this.validation = new ValidationRuleBuilder<>();
		return this.validation;
	}

	@Override
	public void apply(final T instance) {
		final Collection<P> values = this.collectionFunction.apply(instance);
		if (this.when.test(values)) {
			for (final P value : values) {
				this.validation.apply(value);
			}
		}
	}

}
