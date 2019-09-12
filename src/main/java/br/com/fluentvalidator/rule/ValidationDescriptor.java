package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.exception.ValidationException;

interface ValidationDescriptor<P> extends Rule<P> {

	void when(final Predicate<P> when);
	
	void must(final Predicate<P> must);

	void withFieldName(final String fieldName);

	void withMessage(final String message);
	
	void withCode(final String code);
	
	void critical();
	
	void critical(final Class<? extends ValidationException> clazz);

}