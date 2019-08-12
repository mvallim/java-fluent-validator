package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

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

		for (final Rule<T> rule : this.rules) {
			rule.apply(instance);
		}

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
	public <P> WhenProperty<P> ruleFor(final Function<T, P> function) {
		final PropetyRule<T, P> rule = new PropetyRule<>(function);
		this.rules.add(rule);
		return rule;
	}

	@Override
	public <P> WhenCollection<P> ruleForEach(final Function<T, Collection<P>> function) {
		final CollectionPropetyRule<T, P> rule = new CollectionPropetyRule<>(function);
		this.rules.add(rule);
		return rule;
	}

}