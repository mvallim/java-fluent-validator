package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.builder.Code;
import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.RuleBuilder;
import br.com.fluentvalidator.builder.WhenCollection;
import br.com.fluentvalidator.builder.WithValidator;
import br.com.fluentvalidator.exception.ValidationException;

public class CollectionRuleBuilder<T, P> extends AbstractRuleBuilder<T, Collection<P>, WhenCollection<T, P>>
		implements RuleBuilder<T, Collection<P>, WhenCollection<T, P>>, WhenCollection<T, P> {

	private ValidationRuleStore<P, Collection<P>> store = new ValidationRuleStore<>();
	
	public CollectionRuleBuilder(final Function<T, Collection<P>> function) {
		super(function);
	}

	@Override
	public boolean apply(final T instance) {
		return ValidationProcessor.process(this.function.apply(instance), store.getRules());
	}

	@Override
	public Must<T, Collection<P>, WhenCollection<T, P>> must(final Predicate<Collection<P>> predicate) {
		store.getCurrentValidation().must(predicate);
		return this;
	}

	@Override
	public Message<T, Collection<P>, WhenCollection<T, P>> withMessage(final String message) {
		store.getCurrentValidation().withMessage(message);
		return this;
	}

	@Override
	public Code<T, Collection<P>, WhenCollection<T, P>> withCode(final String code) {
		store.getCurrentValidation().withCode(code);
		return this;
	}

	@Override
	public FieldName<T, Collection<P>, WhenCollection<T, P>> withFieldName(final String fieldName) {
		store.getCurrentValidation().withFieldName(fieldName);
		return this;
	}

	@Override
	public Critical<T, Collection<P>, WhenCollection<T, P>> critical() {
		store.getCurrentValidation().critical();
		return this;
	}

	@Override
	public Critical<T, Collection<P>, WhenCollection<T, P>> critical(final Class<? extends ValidationException> clazz) {
		store.getCurrentValidation().critical(clazz);
		return this;
	}

	@Override
	public WithValidator<T, Collection<P>, WhenCollection<T, P>> withValidator(final Validator<P> validator) {
		store.getCurrentValidation().withValidator(validator);
		return this;
	}
	
	@Override
	public WhenCollection<T, P> when(final Predicate<Collection<P>> predicate) {
		store.addRule(new CollectionValidationRule(predicate));
		return this;
	}
	
	class CollectionValidationRule extends ValidationRule<P, Collection<P>> {

		protected CollectionValidationRule(final Predicate<Collection<P>> when) {
			super(when);
		}

		@Override
		boolean accept(final Collection<P> instance) {
			return ValidationProcessor.process(instance, this.getValidator());
		}

	}

}
