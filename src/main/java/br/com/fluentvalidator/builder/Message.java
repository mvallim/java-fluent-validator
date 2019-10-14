package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.exception.ValidationException;

public interface Message<T, P, W extends When<T, P, W>> extends RuleBuilder<T, P, W> {

    /**
     *
     * @param code
     * @return
     */
    Code<T, P, W> withCode(final String code);

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
