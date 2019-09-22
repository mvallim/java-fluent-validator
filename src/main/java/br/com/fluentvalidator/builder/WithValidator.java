package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.exception.ValidationException;

public interface WithValidator<T, P, W extends When<T, P, W>> extends RuleBuilder<T, P, W> {

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
