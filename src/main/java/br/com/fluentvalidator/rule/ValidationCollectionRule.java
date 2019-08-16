package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.Optional;
import java.util.function.Predicate;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.builder.Validator;

class ValidationCollectionRule<P> implements Validation<P, Collection<P>> {

	private Predicate<Collection<P>> must = m -> true;

	private String message;

	private String fieldName;

	private boolean critical;

	private Validator<P> validator;

	public ValidationCollectionRule() {
		super();
	}

	@Override
	public void must(final Predicate<Collection<P>> predicate) {
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
	public boolean apply(final Collection<P> instances) {
		
		boolean apply = this.must.test(instances);
		
		if (Boolean.FALSE.equals(apply)) {
			ValidationContext.get().addError(this.fieldName, this.message, instances);
		}

		if (Optional.ofNullable(validator).isPresent()) {
			for (final P instance : instances) {
				apply &= this.validator.apply(instance);
				if (!apply) break;
			}
		}
		
		return !(Boolean.TRUE.equals(critical) && Boolean.FALSE.equals(apply));
	}

}
