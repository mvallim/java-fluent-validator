package br.com.fluentvalidator.rule;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.RuleProperty;
import br.com.fluentvalidator.builder.WhenProperty;

public class PropetyRule<T, P> implements RuleProperty<T, P> {

	private final Function<T, P> propertyFunction;

	private final List<ValidationHandler<P>> validationHandlers = new LinkedList<>(Arrays.asList(new ValidationHandler<P>(new ValidationPropertyRule<P>())));
	
	private final Function<List<ValidationHandler<P>>, ValidationHandler<P>> last = vh -> vh.get(vh.size() - 1);
	
	private final Function<List<ValidationHandler<P>>, ValidationHandler<P>> first = vh -> vh.get(0);

	public PropetyRule(final Function<T, P> propertyFunction) {
		this.propertyFunction = propertyFunction;
	}

	@Override
	public void addRule(final Predicate<P> predicate, final Rule<P> rule) {
		this.validationHandlers.add(last.apply(validationHandlers).setNextHandler(new ValidationHandler<>(predicate, rule)));
	}

	@Override
	public WhenProperty<T, P> when(final Predicate<P> when) {
		return new InternalRulePropertyBuilder<>(this, when);
	}

	@Override
	public boolean apply(final T instance) {
		return ValidationProcessor.process(this.propertyFunction.apply(instance), first.apply(validationHandlers));
	}
	
}
