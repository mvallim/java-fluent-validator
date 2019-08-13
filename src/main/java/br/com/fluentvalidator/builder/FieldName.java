package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface FieldName<T, P> {

	Critical<T, P> critical();
	
	When<T, P> when(final Predicate<P> predicate);

}
