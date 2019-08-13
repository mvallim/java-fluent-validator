package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Message<T, P> {

	FieldName<T, P> withFieldName(final String fieldName);
	
	Critical<T, P> critical();
	
	When<T, P> when(final Predicate<P> predicate);
	
}
