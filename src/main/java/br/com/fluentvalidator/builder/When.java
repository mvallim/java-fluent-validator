/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.com.fluentvalidator.builder;

import java.util.function.Function;

import br.com.fluentvalidator.exception.ValidationException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

/**
 * Interface for defining conditions that must be satisfied for validation rules to be applied.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 * @param <W> the type of the When condition builder
 * @param <N> the type of the Whenever condition builder
 */
public interface When<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> {

  /**
   * Sets the error code for the validation rule using a static string.
   *
   * @param code the error code to be used in validation messages
   * @return the Code builder for chaining additional configuration
   */
  Code<T, P, W, N> withCode(final String code);

  /**
   * Sets the error code for the validation rule using a function that derives the code from the object.
   *
   * @param code the function to generate the error code from the validated object
   * @return the Code builder for chaining additional configuration
   */
  Code<T, P, W, N> withCode(final Function<T, String> code);

  /**
   * Sets the error message for the validation rule using a static string.
   *
   * @param message the error message to be used when validation fails
   * @return the Message builder for chaining additional configuration
   */
  Message<T, P, W, N> withMessage(final String message);

  /**
   * Sets the error message for the validation rule using a function that derives the message from the object.
   *
   * @param message the function to generate the error message from the validated object
   * @return the Message builder for chaining additional configuration
   */
  Message<T, P, W, N> withMessage(final Function<T, String> message);

  /**
   * Sets the field name for error reporting using a static string.
   *
   * @param fieldName the name of the field to be reported in validation errors
   * @return the FieldName builder for chaining additional configuration
   */
  FieldName<T, P, W, N> withFieldName(final String fieldName);

  /**
   * Sets the field name for error reporting using a function that derives the name from the object.
   *
   * @param fieldName the function to generate the field name from the validated object
   * @return the FieldName builder for chaining additional configuration
   */
  FieldName<T, P, W, N> withFieldName(final Function<T, String> fieldName);

  /**
   * Sets the attempted value to be reported in validation errors.
   *
   * @param attemptedValue the value that failed validation
   * @return the AttemptedValue builder for chaining additional configuration
   */
  AttemptedValue<T, P, W, N> withAttempedValue(final Object attemptedValue);

  /**
   * Sets the attempted value to be reported in validation errors using a function that derives the value from the object.
   *
   * @param attemptedValue the function to extract the attempted value from the validated object
   * @return the AttemptedValue builder for chaining additional configuration
   */
  AttemptedValue<T, P, W, N> withAttempedValue(final Function<T, Object> attemptedValue);

  /**
   * Marks the validation rule as critical, causing validation to stop on failure.
   *
   * @return the Critical builder for chaining additional configuration
   */
  Critical<T, P, W, N> critical();

  /**
   * Marks the validation rule as critical with a custom exception class, causing validation to stop on failure.
   *
   * @param clazz the custom ValidationException class to be thrown on critical failure
   * @return the Critical builder for chaining additional configuration
   */
  Critical<T, P, W, N> critical(final Class<? extends ValidationException> clazz);

  /**
   * Sets a custom handler for invalid field scenarios.
   *
   * @param handlerInvalidField the handler to process invalid field events
   * @return the HandleInvalidField builder for chaining additional configuration
   */
  HandleInvalidField<T, P, W, N> handlerInvalidField(final HandlerInvalidField<P> handlerInvalidField);

}
