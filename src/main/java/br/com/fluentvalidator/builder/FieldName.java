package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface FieldName<T, P, W extends When<T, P, W>> {

	Critical<T, P, W> critical();
	
	W when(final Predicate<P> predicate);

}
