package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface WithValidator<T, P, W extends When<T, P, W>> {

	W when(final Predicate<P> predicate);
	
}
