package br.com.fluentvalidator.rule;

public interface Rule<T> {

  boolean apply(final T instance);

  boolean support(final T instance);

}
