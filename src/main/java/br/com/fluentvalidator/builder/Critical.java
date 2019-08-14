package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Critical<T, P, W extends When<T, P, W>> {

	W when(final Predicate<P> predicate);
	
}
