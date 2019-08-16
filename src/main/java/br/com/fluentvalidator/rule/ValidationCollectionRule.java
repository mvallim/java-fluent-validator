package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.Optional;

import br.com.fluentvalidator.ValidationContext;

class ValidationCollectionRule<P> extends ValidationRule<P, Collection<P>> {

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
		
		boolean apply = this.getMust().test(instances);
		
		if (Boolean.FALSE.equals(apply)) {
			ValidationContext.get().addError(this.getFieldName(), this.getMessage(), instances);
		}

		if (Optional.ofNullable(this.getValidator()).isPresent()) {
			for (final P instance : instances) {
				apply &= this.getValidator().apply(instance);
				if (!apply) break;
			}
		}
		
		return !(Boolean.TRUE.equals(this.isCritical()) && Boolean.FALSE.equals(apply));
	}

}
