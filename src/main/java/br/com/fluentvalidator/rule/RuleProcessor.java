package br.com.fluentvalidator.rule;

import java.util.Collection;

public final class RuleProcessor {

	private RuleProcessor() {
		super();
	}
	
	public static <E> boolean process(final E value, final Rule<E> rule) {
		return Boolean.FALSE.equals(rule.support(value)) || rule.apply(value);
	}

	public static <E> boolean process(final Collection<E> values, final Rule<E> rule) {
		return values.stream().allMatch(value -> process(value, rule));
	}
	
	public static <E> boolean process(final E value, final Collection<Rule<E>> rules) {
		return rules.stream().allMatch(rule -> process(value, rule));
	}

}
