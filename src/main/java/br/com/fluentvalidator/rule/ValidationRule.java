package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Validator;

abstract class ValidationRule<P, A> implements Validation<P, A> {

	private Predicate<A> must = m -> true;

	private String message;

	private String fieldName;

	private boolean critical;

	private Validator<P> validator;

	public ValidationRule() {
		super();
	}

	public Predicate<A> getMust() {
		return this.must;
	}

	public String getMessage() {
		return this.message;
	}

	public String getFieldName() {
		return this.fieldName;
	}

	public boolean isCritical() {
		return this.critical;
	}

	public Validator<P> getValidator() {
		return this.validator;
	}

	@Override
	public void must(final Predicate<A> predicate) {
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

}
