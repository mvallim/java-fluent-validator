package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

import br.com.fluentvalidator.handler.HandlerInvalidField;

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

	/**
	 *
	 * @param handlerInvalidField
	 * @return
	 */
	HandleInvalidField<T, P, W> handlerInvalidField(final HandlerInvalidField<P> handlerInvalidField);

}
