package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

import br.com.fluentvalidator.rule.Rule;

interface RuleBuilder<T, P, W extends When<T, P, W>> extends Rule<T> {
	
	W whenever(final Predicate<P> predicate);
	
	Must<T, P, W> must(final Predicate<P> predicate);
	
}
