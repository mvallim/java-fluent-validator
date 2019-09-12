package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.ValidationException;

interface ValidatorDescriptor<P> extends Rule<P> {
	
	void whenever(final Predicate<P> when);
	
	<T> void withValidator(final Validator<T> validator);
	
	void critical();
	
	void critical(final Class<? extends ValidationException> clazz);

}