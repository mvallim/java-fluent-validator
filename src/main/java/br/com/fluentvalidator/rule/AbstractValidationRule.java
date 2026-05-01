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

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.exception.ValidationException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

/**
 * Abstract base class for validation rules.
 * Implements common logic for when, must, whenever conditions and error handling.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 */
@SuppressWarnings("unchecked")
abstract class AbstractValidationRule<T, P> implements ValidationRule<T, P>, FieldDescriptor<Object, P> {

  private Predicate<P> whenever = w -> true;

  private Predicate<P> when = w -> true;

  private Predicate<P> must = m -> true;

  private Function<Object, String> message = obj -> null;

  private Function<Object, String> code = obj -> null;

  private Function<Object, String> fieldName = obj -> null;

  private Function<Object, Object> attemptedValue;

  private boolean critical;

  private Class<? extends ValidationException> criticalException;

  private Validator<T> validator = new InternalValidator();

  private HandlerInvalidField<P> handlerInvalidField = new InternalHandlerInvalidField(this);

  /**
   * Returns the whenever predicate that determines when this rule should be applied.
   *
   * @return the whenever predicate
   */
  public Predicate<P> getWhenever() {
    return whenever;
  }

  /**
   * Returns the when predicate that must be true for validation to proceed.
   *
   * @return the when predicate
   */
  public Predicate<P> getWhen() {
    return when;
  }

  /**
   * Returns the must predicate that must be satisfied for validation to pass.
   *
   * @return the must predicate
   */
  public Predicate<P> getMust() {
    return must;
  }

  /**
   * Returns the custom exception class for critical validation failures.
   *
   * @return the critical exception class, or null if not set
   */
  public Class<? extends ValidationException> getCriticalException() {
    return criticalException;
  }

  /**
   * Returns the validator associated with this rule.
   *
   * @return the validator
   */
  public Validator<T> getValidator() {
    return validator;
  }

  /**
   * Returns the error message for the given instance.
   *
   * @param instance the instance to get the message for
   * @return the error message
   */
  @Override
  public String getMessage(final Object instance) {
    return message.apply(instance);
  }

  /**
   * Returns the error code for the given instance.
   *
   * @param instance the instance to get the code for
   * @return the error code
   */
  @Override
  public String getCode(final Object instance) {
    return code.apply(instance);
  }

  /**
   * Returns the field name for the given instance.
   *
   * @param instance the instance to get the field name for
   * @return the field name
   */
  @Override
  public String getFieldName(final Object instance) {
    return fieldName.apply(instance);
  }

  /**
   * Returns the attempted value for the given instance.
   *
   * @param instance the instance to get the attempted value for
   * @param defaultValue the default value to return if no attempted value function is set
   * @return the attempted value
   */
  @Override
  public Object getAttemptedValue(final Object instance, final P defaultValue) {
    return Objects.isNull(attemptedValue) ? defaultValue : attemptedValue.apply(instance);
  }

  /**
   * Returns the handler for invalid field scenarios.
   *
   * @return the handler invalid field
   */
  public HandlerInvalidField<P> getHandlerInvalid() {
    return handlerInvalidField;
  }

  /**
   * Returns whether this rule is marked as critical.
   *
   * @return true if critical, false otherwise
   */
  public boolean isCritical() {
    return critical;
  }

  /**
   * Sets the when predicate that must be true for validation to proceed.
   *
   * @param when the when predicate
   */
  @Override
  public void when(final Predicate<P> when) {
    this.when = when;
  }

  /**
   * Sets the must predicate that must be satisfied for validation to pass.
   *
   * @param must the must predicate
   */
  @Override
  public void must(final Predicate<P> must) {
    this.must = must;
  }

  /**
   * Sets the function to generate the field name for error reporting.
   *
   * @param fieldName the function to generate the field name
   */
  @Override
  public void withFieldName(final Function<?, String> fieldName) {
    this.fieldName = (Function<Object, String>) fieldName;
  }

  /**
   * Sets the function to generate the error message for validation failures.
   *
   * @param message the function to generate the error message
   */
  @Override
  public void withMessage(final Function<?, String> message) {
    this.message = (Function<Object, String>) message;
  }

  /**
   * Sets the function to generate the error code for validation failures.
   *
   * @param code the function to generate the error code
   */
  @Override
  public void withCode(final Function<?, String> code) {
    this.code = (Function<Object, String>) code;
  }

  /**
   * Sets the function to generate the attempted value for error reporting.
   *
   * @param attemptedValue the function to generate the attempted value
   */
  @Override
  public void withAttemptedValue(final Function<?, Object> attemptedValue) {
    this.attemptedValue = (Function<Object, Object>) attemptedValue;
  }

  /**
   * Sets a custom handler for invalid field scenarios.
   *
   * @param handlerInvalidField the handler to process invalid field events
   */
  @Override
  public void withHandlerInvalidField(final HandlerInvalidField<P> handlerInvalidField) {
    this.handlerInvalidField = handlerInvalidField;
  }

  /**
   * Marks the validation rule as critical, causing validation to stop on failure.
   */
  @Override
  public void critical() {
    critical = true;
  }

  /**
   * Marks the validation rule as critical with a custom exception class.
   *
   * @param clazz the custom ValidationException class to be thrown on critical failure
   */
  @Override
  public void critical(final Class<? extends ValidationException> clazz) {
    critical = true;
    criticalException = clazz;
  }

  /**
   * Sets the whenever predicate that determines when this rule should be applied.
   *
   * @param whenever the whenever predicate
   */
  @Override
  public void whenever(final Predicate<P> whenever) {
    this.whenever = whenever;
  }

  /**
   * Associates a validator to be applied to the property value.
   *
   * @param validator the validator to apply
   */
  @Override
  public void withValidator(final Validator<T> validator) {
    this.validator = validator;
  }

  /**
   * Internal validator that does nothing. Used as a default validator.
   */
  private class InternalValidator extends AbstractValidator<T> {
    @Override
    public void rules() {
      // Do nothing
    }
  }

  /**
   * Internal handler for invalid field scenarios.
   * Creates an Error object based on the field descriptor.
   */
  private class InternalHandlerInvalidField implements HandlerInvalidField<P> {

    private final FieldDescriptor<Object, P> fieldDescriptor;

    /**
     * Constructs a new InternalHandlerInvalidField.
     *
     * @param fieldDescriptor the field descriptor to use for error details
     */
    public InternalHandlerInvalidField(final FieldDescriptor<Object, P> fieldDescriptor) {
      this.fieldDescriptor = fieldDescriptor;
    }

    @Override
    public Collection<Error> handle(final Object instance, final P attemptedValue) {
      return Collections.singletonList(Error.create(fieldDescriptor.getFieldName(instance), fieldDescriptor.getMessage(instance), fieldDescriptor.getCode(instance), fieldDescriptor.getAttemptedValue(instance, attemptedValue)));
    }

  }

}
