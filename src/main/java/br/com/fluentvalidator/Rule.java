package br.com.fluentvalidator;

public interface Rule<T> {

	void apply(T instance);

}
