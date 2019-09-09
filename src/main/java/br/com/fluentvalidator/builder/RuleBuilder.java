package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface RuleBuilder<T, P, W extends When<T, P, W>> extends Rule<T> {
	
	W when(final Predicate<P> predicate);
	
}
