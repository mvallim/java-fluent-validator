package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

import br.com.fluentvalidator.exception.ValidationException;

public interface FieldName<T, P, W extends When<T, P, W>> {

	Critical<T, P, W> critical();
	
	Critical<T, P, W> critical(final Class<? extends ValidationException> clazz);
	
	W when(final Predicate<P> predicate);

}
