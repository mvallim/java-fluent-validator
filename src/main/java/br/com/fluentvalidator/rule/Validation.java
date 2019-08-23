package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.Validator;
import br.com.fluentvalidator.exception.ValidationException;

interface Validation<T, P> extends Rule<P> {

	void must(final Predicate<? super P> predicate);

	void withFieldName(final String fieldName);

	void withMessage(final String message);
	
	void withCode(final String code);
	
	void withValidator(final Validator<T> validator);

	void critical();
	
	void critical(final Class<? extends ValidationException> clazz);

}