package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface RuleProperty<T, P> extends RuleBuilder<T, P> {
		
	WhenProperty<T, P> when(final Predicate<P> predicate);
	
}
