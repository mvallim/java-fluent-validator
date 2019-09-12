package br.com.fluentvalidator.rule;

import java.util.Objects;
import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.ValidationException;

class ValidatorDescriptorImpl<P> implements ValidatorDescriptor<P> {

	private Predicate<P> whenever = w -> true;
	
	private boolean critical;
	
	private Class<? extends ValidationException> criticalException;

	private Validator<?> validator;

	protected ValidatorDescriptorImpl(final Predicate<P> whenever) {
		this.whenever = whenever;
	}

	public Predicate<P> getWhen() {
		return this.whenever;
	}

	public boolean isCritical() {
		return this.critical;
	}

	@Override
	public void whenever(final Predicate<P> when) {
		this.whenever = when;
	}

	@Override
	public void critical() {
		this.critical = true;
	}
	
	@Override
	public <T> void withValidator(final Validator<T> validator) {
		this.validator = validator;
	}

	@Override
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
		
		boolean apply = this.validator.apply(instance);
		
		if (Objects.nonNull(criticalException) && Boolean.FALSE.equals(apply)) {
			throw ValidationException.create(criticalException);
		}
		
		return !(Boolean.TRUE.equals(this.isCritical()) && Boolean.FALSE.equals(apply));
	}
	
}
