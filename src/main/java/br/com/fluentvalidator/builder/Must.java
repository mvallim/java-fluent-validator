package br.com.fluentvalidator.builder;

public interface Must<T, P> {
	
	Message<T, P> withMessage(final String message);

}
