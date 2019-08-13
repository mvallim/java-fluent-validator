package br.com.fluentvalidator.builder;

public interface Rule<T> {

	void apply(T instance);

}
