package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Must<T, P, W extends When<T, P, W>> {
	
	When<T, P, W> when(final Predicate<P> when);

	Message<T, P, W> withMessage(final String message);
	
}
