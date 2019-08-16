package br.com.fluentvalidator.rule;

import java.util.Optional;

import br.com.fluentvalidator.ValidationContext;

class ValidationPropertyRule<P> extends ValidationRule<P, P> {

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
			ValidationContext.get().addError(this.getFieldName(), this.getMessage(), instance);
		}

		if (Optional.ofNullable(this.getValidator()).isPresent()) {
			apply = this.getValidator().apply(instance);
		}
		
		return !(Boolean.TRUE.equals(this.isCritical()) && Boolean.FALSE.equals(apply));
	}

}
