package br.com.fluentvalidator.rule;

class ValidationPropertyRule<P> extends ValidationRule<P, P> {

	@Override
	boolean applyValidator(final P instance) {
		return ValidationProcessor.process(instance, this.getValidator());
	}

}
