package br.com.fluentvalidator.rule;

import java.util.Objects;
import java.util.function.Predicate;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.exception.ValidationException;

class ValidationDescriptorImpl<P> implements ValidationDescriptor<P> {

	private Predicate<P> when = w -> true;
	
	private Predicate<P> must = m -> true;

	private String message;
	
	private String code;

	private String fieldName;

	private boolean critical;
	
	private Class<? extends ValidationException> criticalException;

	protected ValidationDescriptorImpl(final Predicate<P> must) {
		this.must = must;
	}

	public Predicate<P> getWhen() {
		return this.when;
	}

	public Predicate<P> getMust() {
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

	@Override
	public void when(Predicate<P> when) {
		this.when = when;
	}

	@Override
	public void must(final Predicate<P> must) {
		this.must = must;
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
	public void critical() {
		this.critical = true;
	}
	
	public void critical(final Class<? extends ValidationException> clazz) {
		this.criticalException = clazz;
	}
	
	@Override
	public boolean support(final P instance) {
		return Boolean.TRUE.equals(this.getWhen().test(instance));
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
		
		boolean apply = this.getMust().test(instance);
		
		if (Boolean.FALSE.equals(apply)) {
			ValidationContext.get().addError(this.getFieldName(), this.getMessage(), this.getCode(), instance);
		}
				
		if (Objects.nonNull(criticalException) && Boolean.FALSE.equals(apply)) {
			throw ValidationException.create(criticalException);
		}
		
		return !(Boolean.TRUE.equals(this.isCritical()) && Boolean.FALSE.equals(apply));
	}
	
}
