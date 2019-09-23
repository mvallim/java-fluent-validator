package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.ValidationException;

interface ValidationRule<T, P> extends Rule<P> {

    void when(final Predicate<P> when);

    void must(final Predicate<P> must);

    void withFieldName(final String fieldName);

    void withMessage(final String message);

    void withCode(final String code);

    void critical();

    void critical(final Class<? extends ValidationException> clazz);

    void whenever(final Predicate<P> whenever);

    void withValidator(final Validator<T> validator);

}