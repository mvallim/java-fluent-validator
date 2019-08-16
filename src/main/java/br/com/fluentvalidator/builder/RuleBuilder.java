package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface RuleBuilder<T, P> extends Rule<T> {
	
	void addRule(final Predicate<P> predicate, final Rule<P> rule);
	
	default boolean stopChain(final boolean apply) {
		return Boolean.FALSE.equals(apply);
	}
	
}
