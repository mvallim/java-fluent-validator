package br.com.fluentvalidator.builder;

import java.util.function.Function;
import br.com.fluentvalidator.exception.ValidationException;

public interface AttemptedValue<T, P, W extends When<T, P, W>> extends RuleBuilder<T, P, W> {

  /**
   *
   * @param code
   * @return
   */
  Code<T, P, W> withCode(final String code);

  /**
   *
   * @param code
   * @return
   */
  Code<T, P, W> withCode(final Function<T, String> code);

  /**
   *
   * @param message
   * @return
   */
  Message<T, P, W> withMessage(final String message);

  /**
   *
   * @param message
   * @return
   */
  Message<T, P, W> withMessage(final Function<T, String> message);

  /**
   *
   * @param fieldName
   * @return
   */
  FieldName<T, P, W> withFieldName(final String fieldName);

  /**
   *
   * @param fieldName
   * @return
   */
  FieldName<T, P, W> withFieldName(final Function<T, String> fieldName);

  /**
   *
   * @return
   */
  Critical<T, P, W> critical();

  /**
   *
   * @param clazz
   * @return
   */
  Critical<T, P, W> critical(final Class<? extends ValidationException> clazz);


}
