package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Critical<T, P> {

	When<T, P> when(final Predicate<P> predicate);
	
}
