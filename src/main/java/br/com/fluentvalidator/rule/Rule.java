package br.com.fluentvalidator.rule;

public interface Rule<T> {

	<P> boolean apply(final P instance);
	
	boolean support(final T instance);

}
