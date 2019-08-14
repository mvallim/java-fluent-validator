package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.RuleCollection;
import br.com.fluentvalidator.builder.WhenCollection;

public class CollectionRule<T, P> implements RuleCollection<T, P> {

	private final Function<T, Collection<P>> collectionFunction;

	private final List<RuleEntry<Collection<P>, Collection<P>>> ruleEntries;
	
	public CollectionRule(final Function<T, Collection<P>> collectionFunction) {
		this.collectionFunction = collectionFunction;
		this.ruleEntries = new LinkedList<>();
	}

	@Override
	public void addRule(final Predicate<Collection<P>> predicate, final Rule<Collection<P>> rule) {
		this.ruleEntries.add(new RuleEntry<>(predicate, rule));
	}
	
	@Override
	public WhenCollection<T, P> when(final Predicate<Collection<P>> when) {
		return new InternalRuleCollectionBuilder<>(this, when);
	}

	@Override
	public void apply(final T instance) {
		final Collection<P> values = this.collectionFunction.apply(instance);
		for (final RuleEntry<Collection<P>, Collection<P>> ruleEntry : ruleEntries) {
			if (ruleEntry.getWhen().test(values)) {
				ruleEntry.getRule().apply(values);	
			}
		}
	}

}
