package br.com.fluentvalidator.rule;

import java.util.Collection;

class ValidationCollectionRule<P> extends ValidationRule<P, Collection<P>> {

	@Override
	boolean applyValidator(final Collection<P> instance) {
		return ValidationProcessor.process(instance, this.getValidator());
	}

}
