package br.com.fluentvalidator.builder;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;

@SuppressWarnings({ "rawtypes", "unchecked" })
public class PropetyRule<T, P> implements RuleBuilder<T, P>, RuleProperty<T, P> {

	private final Function<T, P> propertyFunction;

	private final Map<Predicate<P>, Rule> rules;

	public PropetyRule(final Function<T, P> propertyFunction) {
		this.propertyFunction = propertyFunction;
		this.rules = new ConcurrentHashMap<>();
	}

	@Override
	public void addRule(final Predicate<P> predicate, final Rule<?> rule) {
		this.rules.put(predicate, rule);
	}

	@Override
	public WhenProperty<T, P> when(final Predicate<P> predicate) {
		return new InternalRulePropertyBuilder<>(this, predicate);
	}
	
	@Override
	public void apply(final T instance) {
		final P value = this.propertyFunction.apply(instance);
		for (final Entry<Predicate<P>, Rule> rule : rules.entrySet()) {
			if (rule.getKey().test(value)) {
				rule.getValue().apply(value);
			}
		}
	}

}
