package br.com.fluentvalidator.rule;

public interface Rule<T> {

  /**
   *
   * @param instance
   * @return
   */
  boolean apply(final T instance);

  /**
   *
   * @param instance
   * @return
   */
  boolean support(final T instance);

}
