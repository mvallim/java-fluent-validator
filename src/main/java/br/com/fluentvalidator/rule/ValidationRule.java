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

package br.com.fluentvalidator.rule;

import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.ValidationException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

/**
 * Interface for validation rules that can be configured with conditions and error handling.
 * Extends Rule to provide fluent configuration options.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 */
interface ValidationRule<T, P> extends Rule<P> {

  /**
   * Sets the condition that must be true for the validation to be applied.
   *
   * @param when the predicate that must be true
   */
  void when(final Predicate<P> when);

  /**
   * Sets the predicate that must be satisfied for the validation to pass.
   *
   * @param must the predicate to validate against
   */
  void must(final Predicate<P> must);

  /**
   * Sets the function to generate the field name for error reporting.
   *
   * @param fieldName the function to generate the field name
   */
  void withFieldName(final Function<?, String> fieldName);

  /**
   * Sets the function to generate the error message for validation failures.
   *
   * @param message the function to generate the error message
   */
  void withMessage(final Function<?, String> message);

  /**
   * Sets the function to generate the error code for validation failures.
   *
   * @param code the function to generate the error code
   */
  void withCode(final Function<?, String> code);

  /**
   * Sets the function to generate the attempted value for error reporting.
   *
   * @param attemptedValue the function to generate the attempted value
   */
  void withAttemptedValue(final Function<?, Object> attemptedValue);

  /**
   * Sets a custom handler for invalid field scenarios.
   *
   * @param handleInvalid the handler to process invalid field events
   */
  void withHandlerInvalidField(final HandlerInvalidField<P> handleInvalid);

  /**
   * Marks the validation rule as critical, causing validation to stop on failure.
   */
  void critical();

  /**
   * Marks the validation rule as critical with a custom exception class.
   *
   * @param clazz the custom ValidationException class to be thrown on critical failure
   */
  void critical(final Class<? extends ValidationException> clazz);

  /**
   * Sets the condition that determines when the validation rule should be applied.
   *
   * @param whenever the predicate that determines when to apply the rule
   */
  void whenever(final Predicate<P> whenever);

  /**
   * Associates a validator to be applied to the property value.
   *
   * @param validator the validator to apply
   */
  void withValidator(final Validator<T> validator);

}
