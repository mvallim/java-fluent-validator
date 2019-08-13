package br.com.fluentvalidator;

import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;

public interface Validation<P> extends Rule<P> {

	Validation<P> must(final Predicate<P> predicate);

	Validation<P> withFieldName(final String fieldName);

	Validation<P> withMessage(final String message);
	
	Validation<P> critical();

}