package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.RuleCollection;
import br.com.fluentvalidator.builder.Validator;
import br.com.fluentvalidator.builder.WhenCollection;
import br.com.fluentvalidator.builder.WithValidator;

class InternalRuleCollectionBuilder<T, P> implements 
	WhenCollection<T, P>, 
	Must<T, Collection<P>, WhenCollection<T, P>>, 
	Message<T, Collection<P>, WhenCollection<T, P>>, 
	FieldName<T, Collection<P>, WhenCollection<T, P>>, 
	Critical<T, Collection<P>, WhenCollection<T, P>>, 
	WithValidator<T, Collection<P>, WhenCollection<T, P>> {

	private final RuleCollection<T, P> ruleBuilder;

	private Validation<P, Collection<P>> validation;

	public InternalRuleCollectionBuilder(final RuleCollection<T, P> ruleBuilder, final Predicate<Collection<P>> predicate) {
		this.ruleBuilder = ruleBuilder;
		this.validation = new ValidationCollectionRule<>();
		this.ruleBuilder.addRule(predicate, this.validation);
	}

	@Override
	public FieldName<T, Collection<P>, WhenCollection<T, P>> withFieldName(final String fieldName) {
		this.validation.withFieldName(fieldName);
		return this;
	}

	@Override
	public WhenCollection<T, P> when(final Predicate<Collection<P>> predicate) {
		return this.ruleBuilder.when(predicate);
	}

	@Override
	public Message<T, Collection<P>, WhenCollection<T, P>> withMessage(final String message) {
		this.validation.withMessage(message);
		return this;
	}

	@Override
	public Must<T, Collection<P>, WhenCollection<T, P>> must(final Predicate<Collection<P>> predicate) {
		this.validation.must(predicate);
		return this;
	}

	@Override
	public WithValidator<T, Collection<P>, WhenCollection<T, P>> withValidator(final Validator<P> validator) {
		this.validation.withValidator(validator);
		return this;
	}

	@Override
	public Critical<T, Collection<P>, WhenCollection<T, P>> critical() {
		this.validation.critical();
		return this;
	}

}
