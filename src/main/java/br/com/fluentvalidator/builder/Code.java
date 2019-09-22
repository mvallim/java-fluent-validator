package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.exception.ValidationException;

public interface Code<T, P, W extends When<T, P, W>> extends RuleBuilder<T, P, W> {

    /**
     *
     * @param fieldName
     * @return
     */
    FieldName<T, P, W> withFieldName(final String fieldName);

    /**
     *
     * @return
     */
    Critical<T, P, W> critical();

    /**
     *
     * @param clazz
     * @return
     */
    Critical<T, P, W> critical(final Class<? extends ValidationException> clazz);

}
