package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

class ValidationRuleBuilder<P> implements ValidationBuilder<P> {

	private final List<Validation<P>> validations = new ArrayList<>();

	private final List<Validator<P>> validators = new ArrayList<>();

	private final Function<List<Validation<P>>, Validation<P>> current = v -> v.get(v.size() - 1);

	@Override
	public Validation<P> must(final Predicate<P> predicate) {
		this.validations.add(new ValidationRule<P>());
		this.current.apply(this.validations).must(predicate);
		return this;
	}

	@Override
	public Validation<P> withFieldName(final String fieldName) {
		this.current.apply(this.validations).withFieldName(fieldName);
		return this;
	}

	@Override
	public Validation<P> withMessage(final String message) {
		this.current.apply(this.validations).withMessage(message);
		return this;
	}

	@Override
	public void apply(final P instance) {
		for (final Validation<P> validation : this.validations) {
			validation.apply(instance);
		}

		for (final Validator<P> validator : this.validators) {
			for (final Rule<P> validation : validator.getRules()) {
				validation.apply(instance);
			}
		}
	}

	@Override
	public Validation<P> withValidator(final Validator<P> validator) {
		this.validators.add(validator);
		return this;
	}

}
