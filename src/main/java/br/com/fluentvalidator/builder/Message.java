package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.exception.ValidationException;

public interface Message<T, P, W extends When<T, P, W>> extends RuleBuilder<T, P, W> {
		
	Code<T, P, W> withCode(final String code);

	FieldName<T, P, W> withFieldName(final String fieldName);
	
	Critical<T, P, W> critical();
	
	Critical<T, P, W> critical(final Class<? extends ValidationException> clazz);
	
}
