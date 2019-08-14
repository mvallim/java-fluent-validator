package br.com.fluentvalidator.builder;

public interface Must<T, P, W extends When<T, P, W>> {
	
	Message<T, P, W> withMessage(final String message);

}
