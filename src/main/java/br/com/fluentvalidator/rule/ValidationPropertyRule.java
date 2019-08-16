package br.com.fluentvalidator.rule;

import java.util.Optional;
import java.util.function.Predicate;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.builder.Validator;

class ValidationPropertyRule<P> implements Validation<P, P> {

	private Predicate<P> must = m -> true;

	private String message;

	private String fieldName;

	private boolean critical;

	private Validator<P> validator;

	public ValidationPropertyRule() {
		super();
	}

	@Override
	public void must(final Predicate<P> predicate) {
		this.must = predicate;
	}

	@Override
	public void withFieldName(final String fieldName) {
		this.fieldName = fieldName;
	}

	@Override
	public void withMessage(final String message) {
		this.message = message;
	}

	@Override
	public void withValidator(final Validator<P> validator) {
		this.validator = validator;
	}

	@Override
	public void critical() {
		this.critical = true;
	}

	@Override
	public void apply(final P instance) {
		if (!this.must.test(instance)) {
			ValidationContext.get().addError(this.fieldName, this.message, instance);
		}

		if (Optional.ofNullable(validator).isPresent()) {
			this.validator.apply(instance);
		}
	}

}
