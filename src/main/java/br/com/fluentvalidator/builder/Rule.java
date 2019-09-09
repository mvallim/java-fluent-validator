package br.com.fluentvalidator.builder;

public interface Rule<T> {

	boolean apply(final T instance);

}
