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
 * Base abstract class for validation rules, providing default implementations for configuration methods
 * and field descriptor methods. This class handles storage of rule properties (message, code, field name, etc.)
 * and provides accessors for these properties.
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
   * Returns the predicate that determines when this rule is active.
   *
   * @return the whenever predicate
   */
  public Predicate<P> getWhenever() {
    return this.whenever;
  }

  /**
   * Returns the predicate that determines when this rule should be applied.
   *
   * @return the when predicate
   */
  public Predicate<P> getWhen() {
    return this.when;
  }

  /**
   * Returns the predicate that defines the validation condition.
   *
   * @return the must predicate
   */
  public Predicate<P> getMust() {
    return this.must;
  }

  /**
   * Returns the exception class to throw if this rule is critical and validation fails.
   *
   * @return the critical exception class, or {@code null} if not set
   */
  public Class<? extends ValidationException> getCriticalException() {
    return this.criticalException;
  }

  /**
   * Returns the nested validator associated with this rule, for validating complex property types.
   *
   * @return the nested validator
   */
  public Validator<T> getValidator() {
    return this.validator;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMessage(final Object instance) {
    return this.message.apply(instance);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getCode(final Object instance) {
    return this.code.apply(instance);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getFieldName(final Object instance) {
    return this.fieldName.apply(instance);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getAttemptedValue(final Object instance, final P defaultValue) {
    return Objects.isNull(this.attemptedValue) ? defaultValue : this.attemptedValue.apply(instance);
  }

  /**
   * Returns the handler for invalid field errors.
   *
   * @return the handler invalid field
   */
  public HandlerInvalidField<P> getHandlerInvalid() {
    return handlerInvalidField;
  }

  /**
   * Checks if this rule is marked as critical.
   *
   * @return {@code true} if critical, {@code false} otherwise
   */
  public boolean isCritical() {
    return this.critical;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void when(final Predicate<P> when) {
    this.when = when;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void must(final Predicate<P> must) {
    this.must = must;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void withFieldName(final Function<?, String> fieldName) {
    this.fieldName = (Function<Object, String>) fieldName;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void withMessage(final Function<?, String> message) {
    this.message = (Function<Object, String>) message;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void withCode(final Function<?, String> code) {
    this.code = (Function<Object, String>) code;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void withAttemptedValue(final Function<?, Object> attemptedValue) {
    this.attemptedValue = (Function<Object, Object>) attemptedValue;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void withHandlerInvalidField(final HandlerInvalidField<P> handlerInvalidField) {
    this.handlerInvalidField = handlerInvalidField;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void critical() {
    this.critical = true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void critical(final Class<? extends ValidationException> clazz) {
    this.critical = true;
    this.criticalException = clazz;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void whenever(final Predicate<P> whenever) {
    this.whenever = whenever;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void withValidator(final Validator<T> validator) {
    this.validator = validator;
  }

  /**
   * Internal validator that does not define any rules, used as a default when no nested validator is set.
   */
  private class InternalValidator extends AbstractValidator<T> {
    @Override
    public void rules() {
      // Do nothing
    }
  }

  /**
   * Internal handler for invalid fields that generates error objects using the associated {@link FieldDescriptor}.
   */
  private class InternalHandlerInvalidField implements HandlerInvalidField<P> {

    private final FieldDescriptor<Object, P> fieldDescriptor;

    /**
     * Constructs a new InternalHandlerInvalidField with the given field descriptor.
     *
     * @param fieldDescriptor the field descriptor to use for generating error properties
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
