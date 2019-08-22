package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Code<T, P, W extends When<T, P, W>> {

	FieldName<T, P, W> withFieldName(final String fieldName);
	
	Critical<T, P, W> critical();	
	
	W when(final Predicate<P> predicate);
	
}
