package br.com.fluentvalidator;

import java.util.function.Predicate;

public interface Validation<P> extends Rule<P> {

	Validation<P> must(final Predicate<P> predicate);

	Validation<P> withFieldName(final String fieldName);

	Validation<P> withMessage(final String message);

}