package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.RuleProperty;
import br.com.fluentvalidator.builder.Validator;
import br.com.fluentvalidator.builder.WhenProperty;
import br.com.fluentvalidator.builder.WithValidator;

class InternalRulePropertyBuilder<T, P> implements WhenProperty<T, P>, Must<T, P, WhenProperty<T, P>>, 
	Message<T, P, WhenProperty<T, P>>, FieldName<T, P, WhenProperty<T, P>>, Critical<T, P, WhenProperty<T, P>>,
	WithValidator<T, P, WhenProperty<T, P>> {
	
	private Predicate<P> predicate = (p) -> true;;
	
	private final RuleProperty<T, P> ruleBuilder;

	private Validation<P, P> validation;

	public InternalRulePropertyBuilder(final RuleProperty<T, P> ruleBuilder, final Predicate<P> predicate) {
		this.predicate = predicate;
		this.ruleBuilder = ruleBuilder;
		this.validation = new ValidationPropertyRule<>();
		this.ruleBuilder.addRule(this.predicate, this.validation);
	}
	
	@Override
	public FieldName<T, P, WhenProperty<T, P>> withFieldName(final String fieldName) {
		this.validation.withFieldName(fieldName);
		return this;
	}

	@Override
	public WhenProperty<T, P> when(final Predicate<P> predicate) {
		return this.ruleBuilder.when(predicate);
	}

	@Override
	public Message<T, P, WhenProperty<T, P>> withMessage(final String message) {
		this.validation.withMessage(message);
		return this;
	}

	@Override
	public Must<T, P, WhenProperty<T, P>> must(final Predicate<P> predicate) {
		this.validation.must(predicate);
		return this;
	}

	@Override
	public WithValidator<T, P, WhenProperty<T, P>> withValidator(final Validator<P> validator) {
		this.validation.withValidator(validator);
		return this;
	}

	@Override
	public Critical<T, P, WhenProperty<T, P>> critical() {
		this.validation.critical();
		return this;
	}

}
