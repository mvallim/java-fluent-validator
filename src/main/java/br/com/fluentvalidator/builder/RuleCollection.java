package br.com.fluentvalidator.builder;

import java.util.Collection;
import java.util.function.Predicate;

public interface RuleCollection<T, P> extends RuleBuilder<T, Collection<P>> {
	
	WhenCollection<T, P> when(final Predicate<Collection<P>> predicate);
	
}
