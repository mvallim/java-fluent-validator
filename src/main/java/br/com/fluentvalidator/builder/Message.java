package br.com.fluentvalidator.builder;

import java.util.function.Function;

import br.com.fluentvalidator.exception.ValidationException;

public interface Message<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> extends RuleBuilder<T, P, W, N> {

  /**
   *
   * @param code
   * @return
   */
  Code<T, P, W, N> withCode(final String code);

  /**
   *
   * @param code
   * @return
   */
  Code<T, P, W, N> withCode(final Function<T, String> code);

  /**
   *
   * @param fieldName
   * @return
   */
  FieldName<T, P, W, N> withFieldName(final String fieldName);

  /**
   *
   * @param fieldName
   * @return
   */
  FieldName<T, P, W, N> withFieldName(final Function<T, String> fieldName);

  /**
   *
   * @param fieldName
   * @return
   */
  AttemptedValue<T, P, W, N> withAttempedValue(final Object attemptedValue);

  /**
   *
   * @param fieldName
   * @return
   */
  AttemptedValue<T, P, W, N> withAttempedValue(final Function<T, Object> attemptedValue);

  /**
   *
   * @return
   */
  Critical<T, P, W, N> critical();

  /**
   *
   * @param clazz
   * @return
   */
  Critical<T, P, W, N> critical(final Class<? extends ValidationException> clazz);

}
