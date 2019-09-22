package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.ValidationException;

abstract class AbstractValidationRule<T, P> implements ValidationRule<T, P> {

    private Predicate<P> whenever = w -> true;

    private Predicate<P> when = w -> true;

    private Predicate<P> must = m -> true;

    private String message;

    private String code;

    private String fieldName;

    private boolean critical;

    private Class<? extends ValidationException> criticalException;

    private Validator<T> validator;

    public Predicate<P> getWhenever() {
        return this.whenever;
    }

    public Predicate<P> getWhen() {
        return this.when;
    }

    public Predicate<P> getMust() {
        return this.must;
    }

    public Class<? extends ValidationException> getCriticalException() {
        return this.criticalException;
    }

    public Validator<T> getValidator() {
        return this.validator;
    }

    public String getMessage() {
        return this.message;
    }

    public String getCode() {
        return this.code;
    }

    public String getFieldName() {
        return this.fieldName;
    }

    public boolean isCritical() {
        return this.critical;
    }

    @Override
    public void when(final Predicate<P> when) {
        this.when = when;
    }

    @Override
    public void must(final Predicate<P> must) {
        this.must = must;
    }

    @Override
    public void withFieldName(final String fieldName) {
        this.fieldName = fieldName;
    }

    @Override
    public void withMessage(final String message) {
        this.message = message;
    }

    @Override
    public void withCode(final String code) {
        this.code = code;
    }

    @Override
    public void critical() {
        this.critical = true;
    }

    @Override
    public void critical(final Class<? extends ValidationException> clazz) {
        this.critical = true;
        this.criticalException = clazz;
    }

    @Override
    public void whenever(final Predicate<P> whenever) {
        this.whenever = whenever;
    }

    @Override
    public void withValidator(final Validator<T> validator) {
        this.validator = validator;
    }

}
