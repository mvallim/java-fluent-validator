package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Must<T, P, W extends When<T, P, W>> {

    /**
     *
     * @param when
     * @return
     */
    When<T, P, W> when(final Predicate<P> when);

    /**
     *
     * @param message
     * @return
     */
    Message<T, P, W> withMessage(final String message);

}
