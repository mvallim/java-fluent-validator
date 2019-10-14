package br.com.fluentvalidator.context;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;

import br.com.fluentvalidator.exception.ValidationException;

public final class ValidationResult {

    private final boolean valid;

    private final Collection<Error> errors;

    /**
     *
     * @return
     */
    public static ValidationResult ok() {
        return new ValidationResult(true, new ArrayList<>());
    }

    /**
     *
     * @param messages
     * @return
     */
    public static ValidationResult fail(final Collection<Error> messages) {
        return new ValidationResult(false, Optional.ofNullable(messages).orElse(new ArrayList<>()));
    }

    private ValidationResult(final boolean valid, final Collection<Error> messages) {
        this.valid = valid;
        errors = Collections.unmodifiableCollection(messages);
    }

    /**
     *
     * @param clazz
     */
    public <T extends ValidationException> void isInvalidThrow(final Class<T> clazz) {
        if (!isValid()) {
            throw ValidationException.create(clazz, this);
        }
    }

    /**
     *
     * @return
     */
    public boolean isValid() {
        return valid;
    }

    /**
     *
     * @return
     */
    public Collection<Error> getErrors() {
        return errors;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append("ValidationResult [valid=");
        builder.append(valid);
        builder.append(", ");
        builder.append("errors=");
        builder.append(errors);
        builder.append("]");
        return builder.toString();
    }

}