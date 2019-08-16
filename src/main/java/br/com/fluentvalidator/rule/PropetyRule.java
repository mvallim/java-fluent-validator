package br.com.fluentvalidator.rule;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.RuleProperty;
import br.com.fluentvalidator.builder.WhenProperty;

public class PropetyRule<T, P> implements RuleProperty<T, P> {

	private final Function<T, P> propertyFunction;

	private final List<RuleEntry<P, P>> ruleEntries;

	public PropetyRule(final Function<T, P> propertyFunction) {
		this.propertyFunction = propertyFunction;
		this.ruleEntries = new LinkedList<>();
	}

	@Override
	public void addRule(final Predicate<P> predicate, final Rule<P> rule) {
		this.ruleEntries.add(new RuleEntry<>(predicate, rule));
	}

	@Override
	public WhenProperty<T, P> when(final Predicate<P> when) {
		return new InternalRulePropertyBuilder<>(this, when);
	}

	@Override
	public boolean apply(final T instance) {
		final P value = this.propertyFunction.apply(instance);
		for (final RuleEntry<P, P> ruleEntry : ruleEntries) {
			if (ruleEntry.getWhen().test(value)) {
				if (stopChain(ruleEntry.getRule().apply(value))) return false;
			}
		}
		return true;
	}
	
}
