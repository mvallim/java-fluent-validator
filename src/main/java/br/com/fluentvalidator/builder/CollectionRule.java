package br.com.fluentvalidator.builder;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;

@SuppressWarnings({ "rawtypes", "unchecked" })
public class CollectionRule<T, P> implements RuleBuilder<T, Collection<P>>, RuleCollection<T, P> {

	private final Function<T, Collection<P>> collectionFunction;

	private final Map<Predicate<Collection<P>>, Rule> rules;

	public CollectionRule(final Function<T, Collection<P>> collectionFunction) {
		this.collectionFunction = collectionFunction;
		this.rules = new ConcurrentHashMap<>();
	}

	@Override
	public void addRule(Predicate<Collection<P>> predicate, Rule<?> rule) {
		this.rules.put(predicate, rule);
	}

	@Override
	public WhenCollection<T, P> when(final Predicate<Collection<P>> predicate) {
		return new InternalRuleCollectionBuilder<>(this, predicate);
	}

	@Override
	public void apply(final T instance) {
		final Collection<P> values = this.collectionFunction.apply(instance);
		for (final Entry<Predicate<Collection<P>>, Rule> rule : rules.entrySet()) {
			if (rule.getKey().test(values)) {
				for (final P value : values) {
					rule.getValue().apply(value);	
				}
			}
		}
	}

}
