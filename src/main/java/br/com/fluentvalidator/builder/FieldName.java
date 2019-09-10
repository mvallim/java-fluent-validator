package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.exception.ValidationException;

public interface FieldName<T, P, W extends When<T, P, W>> extends RuleBuilder<T, P, W> {

	Critical<T, P, W> critical();
	
	Critical<T, P, W> critical(final Class<? extends ValidationException> clazz);
	
}
