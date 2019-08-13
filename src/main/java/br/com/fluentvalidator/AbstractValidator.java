package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.builder.CollectionRule;
import br.com.fluentvalidator.builder.PropetyRule;
import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.Validator;
import br.com.fluentvalidator.builder.RuleCollection;
import br.com.fluentvalidator.builder.RuleProperty;

public abstract class AbstractValidator<T> implements Validator<T> {

	private final List<Rule<T>> rules = new LinkedList<>();

	private String property;

	protected AbstractValidator() {
		this.rules();
	}

	protected abstract void rules();

	public void setPropertyOnContext(final String property) {
		this.property = property;
	}

	public <P> P getPropertyOnContext(final String property, final Class<P> clazz) {
		return ValidationContext.get().getProperty(property, clazz);
	}

	@Override
	public Collection<Rule<T>> getRules() {
		return Collections.unmodifiableCollection(this.rules);
	}

	@Override
	public ValidationResult validate(final T instance) {
		ValidationContext.get().addProperty(this.property, instance);
		apply(instance);
		return ValidationContext.get().getValidationResult();
	}

	@Override
	public List<ValidationResult> validate(final Collection<T> instances) {
		final List<ValidationResult> results = new ArrayList<>();
		for (final T instance : instances) {
			results.add(this.validate(instance));
		}
		return Collections.unmodifiableList(results);
	}
	
	@Override
	public void apply(final T instance) {
		for (final Rule<T> rule : this.rules) {
			rule.apply(instance);
		}		
	}

	@Override
	public <P> RuleProperty<T, P> ruleFor(final Function<T, P> function) {
		final PropetyRule<T, P> rule = new PropetyRule<>(function);
		this.rules.add(rule);
		return rule;
	}

	@Override
	public <P> RuleCollection<T, P> ruleForEach(final Function<T, Collection<P>> function) {
		final CollectionRule<T, P> rule = new CollectionRule<>(function);
		this.rules.add(rule);
		return rule;
	}

}