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

	/*
	 * +----------+-----------+--------+
	 * | critical | composite | result |
	 * +----------+-----------+--------|
	 * | true     | true      | true   |
	 * | true     | false     | false  |
	 * | false    | true      | true   |
	 * | false    | false     | true   |
	 * +----------+-----------+--------+
	 */
	@Override
	public boolean apply(final P instance) {
		boolean apply = true;
		
		if (!(apply = this.must.test(instance))) {
			ValidationContext.get().addError(this.fieldName, this.message, instance);
		}

		if (Optional.ofNullable(validator).isPresent()) {
			apply = this.validator.apply(instance);
		}
		
		return !(Boolean.TRUE.equals(critical) && Boolean.FALSE.equals(apply));
	}

}
