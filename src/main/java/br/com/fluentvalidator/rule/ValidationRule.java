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
 * A validation rule that extends {@link Rule} with additional configuration methods for property validation.
 * This interface provides methods to set validation conditions, error messages, codes, and other rule properties.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 */
interface ValidationRule<T, P> extends Rule<P> {

  /**
   * Sets the predicate that determines when this rule should be applied.
   * The rule is only applied if the predicate returns {@code true} for the property value.
   *
   * @param when the predicate to check before applying the rule
   */
  void when(final Predicate<P> when);

  /**
   * Sets the predicate that defines the validation condition.
   * The rule is satisfied if this predicate returns {@code true} for the property value.
   *
   * @param must the validation predicate
   */
  void must(final Predicate<P> must);

  /**
   * Sets the function to generate the field name associated with this rule, used in error messages.
   *
   * @param fieldName the function to generate the field name
   */
  void withFieldName(final Function<?, String> fieldName);

  /**
   * Sets the function to generate the error message associated with this rule.
   *
   * @param message the function to generate the error message
   */
  void withMessage(final Function<?, String> message);

  /**
   * Sets the function to generate the error code associated with this rule.
   *
   * @param code the function to generate the error code
   */
  void withCode(final Function<?, String> code);

  /**
   * Sets the function to generate the attempted value (the value that failed validation) for error reporting.
   *
   * @param attemptedValue the function to generate the attempted value
   */
  void withAttemptedValue(final Function<?, Object> attemptedValue);

  /**
   * Sets the handler for invalid field errors, which generates error objects when validation fails.
   *
   * @param handleInvalid the handler for invalid fields
   */
  void withHandlerInvalidField(final HandlerInvalidField<P> handleInvalid);

  /**
   * Marks this rule as critical. If validation fails, a {@link ValidationException} will be thrown immediately.
   */
  void critical();

  /**
   * Marks this rule as critical, throwing the specified exception class if validation fails.
   *
   * @param clazz the type of exception to throw when validation fails
   */
  void critical(final Class<? extends ValidationException> clazz);

  /**
   * Sets the predicate that determines the scope of this rule.
   * The rule is only active if the predicate returns {@code true} for the property value.
   *
   * @param whenever the predicate to check for rule activation
   */
  void whenever(final Predicate<P> whenever);

  /**
   * Associates a validator to apply to the property value, for nested validation of complex objects.
   *
   * @param validator the nested validator to apply
   */
  void withValidator(final Validator<T> validator);

}
