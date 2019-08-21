package br.com.fluentvalidator.rule;

import java.util.Optional;
import java.util.function.Predicate;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.builder.Validator;

abstract class ValidationRule<P, A> implements Validation<P, A> {

	private Predicate<A> must = m -> true;

	private String message;
	
	private String code;

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
	
	public String getCode() {
		return code;
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
	public void withCode(final String code) {
		this.code = code;
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
	public boolean apply(final A instance) {
		
		boolean apply = this.getMust().test(instance);
		
		if (Boolean.FALSE.equals(apply)) {
			ValidationContext.get().addError(this.getFieldName(), this.getMessage(), this.getCode(), instance);
		}
		
		if (Optional.ofNullable(this.getValidator()).isPresent()) {
			apply = applyValidator(instance);
		}
		
		return !(Boolean.TRUE.equals(this.isCritical()) && Boolean.FALSE.equals(apply));
	}
	
	abstract boolean applyValidator(final A instance);

}
