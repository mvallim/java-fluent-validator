package br.com.fluentvalidator;

import java.util.function.Predicate;

class ValidationRule<P> implements Validation<P> {

	private Predicate<P> must;

	private String message;

	private String fieldName;

	public ValidationRule() {
		super();
	}

	@Override
	public Validation<P> must(final Predicate<P> predicate) {
		this.must = predicate;
		return this;
	}

	@Override
	public Validation<P> withFieldName(final String fieldName) {
		this.fieldName = fieldName;
		return this;
	}

	@Override
	public Validation<P> withMessage(final String message) {
		this.message = message;
		return this;
	}

	@Override
	public void apply(final P instance) {
		if (!this.must.test(instance)) {
			ValidationContext.get().addError(this.fieldName, this.message, instance);
		}
	}

}
