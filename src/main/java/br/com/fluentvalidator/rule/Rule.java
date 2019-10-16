package br.com.fluentvalidator.rule;

public interface Rule<T> {

  default boolean apply(final T instance) {
    return true;
  }

  /**
   *
   * @param obj
   * @param value
   * @return
   */
  default boolean apply(final Object instance, final T value) {
    return apply(value);
  }

  /**
   *
   * @param instance
   * @return
   */
  default boolean support(final T instance) {
    return true;
  }

}
