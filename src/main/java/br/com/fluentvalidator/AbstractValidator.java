package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.RuleCollection;
import br.com.fluentvalidator.builder.RuleProperty;
import br.com.fluentvalidator.builder.Validator;
import br.com.fluentvalidator.rule.CollectionRule;
import br.com.fluentvalidator.rule.PropetyRule;
import br.com.fluentvalidator.rule.ValidationProcessor;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public abstract class AbstractValidator<T> implements Validator<T> {

	private final List<Rule<T>> rules = new LinkedList<>();
	
	private final Runnable initialize;

	private String property;
	
	protected AbstractValidator() {
		this.initialize = new Runnable() {
			
			private boolean initialized;
			
			@Override
			public synchronized void run() {
				if (!initialized) {
					rules();
					this.initialized = true;
				}
			}
			
		};
	}
	
	protected abstract void rules();
	
	public void setPropertyOnContext(final String property) {
		this.property = property;
	}

	public <P> P getPropertyOnContext(final String property, final Class<P> clazz) {
		return ValidationContext.get().getProperty(property, clazz);
	}
		
	@Override
	public ValidationResult validate(final T instance) {
		ValidationProcessor.process(instance, this);
		return ValidationContext.get().getValidationResult();
	}

	@Override
	public <E> E validate(final T instance, final ValidationResultTransform<E> resultTransform) {
		return resultTransform.transform(validate(instance));
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
	public <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> resultTransform) {
		final List<E> results = new ArrayList<>();
		for (final ValidationResult validationResult : validate(instances)) {
			results.add(resultTransform.transform(validationResult));
		}
		return results;
	}

	@Override
	public boolean apply(final T instance) {
		this.initialize.run();
		ValidationContext.get().setProperty(this.property, instance);
		return ValidationProcessor.process(instance, rules);
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