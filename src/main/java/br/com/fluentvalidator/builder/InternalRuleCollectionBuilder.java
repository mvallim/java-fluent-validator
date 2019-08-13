package br.com.fluentvalidator.builder;

import java.util.Collection;
import java.util.function.Predicate;

import br.com.fluentvalidator.Validation;
import br.com.fluentvalidator.ValidationRule;

class InternalRuleCollectionBuilder<T, P> implements WhenCollection<T, P>, Must<T, Collection<P>>, Message<T, Collection<P>>, FieldName<T, Collection<P>>, Critical<T, Collection<P>>, WithValidator<T, Collection<P>> {

	private Predicate<Collection<P>> predicate;

	private final RuleCollection<T, P> ruleBuilder;

	private Validation<Collection<P>> validation;

	public InternalRuleCollectionBuilder(final RuleCollection<T, P> ruleBuilder, final Predicate<Collection<P>> predicate) {
		this.predicate = predicate;
		this.ruleBuilder = ruleBuilder;
		this.validation = new ValidationRule<>();
		this.ruleBuilder.addRule(this.predicate, this.validation);
	}

	@Override
	public FieldName<T, Collection<P>> withFieldName(final String fieldName) {
		this.validation.withFieldName(fieldName);
		return this;
	}

	@Override
	public When<T, Collection<P>> when(final Predicate<Collection<P>> predicate) {
		return this.ruleBuilder.when(predicate);
	}

	@Override
	public Message<T, Collection<P>> withMessage(final String message) {
		this.validation.withMessage(message);
		return this;
	}

	@Override
	public Must<T, Collection<P>> must(final Predicate<Collection<P>> predicate) {
		this.validation.must(predicate);
		return this;
	}

	@Override
	public WithValidator<T, Collection<P>> withValidator(final Validator<P> validator) {
		this.ruleBuilder.addRule(this.predicate, validator);
		return this;
	}

	@Override
	public Critical<T, Collection<P>> critical() {
		this.validation.critical();
		return this;
	}

}
