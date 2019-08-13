package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface WithValidator<T, P> {

	When<T, P> when(final Predicate<P> predicate);
	
}
