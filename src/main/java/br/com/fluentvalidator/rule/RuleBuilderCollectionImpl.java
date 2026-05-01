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
import java.util.LinkedList;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.builder.AttemptedValue;
import br.com.fluentvalidator.builder.Code;
import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.HandleInvalidField;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.WhenCollection;
import br.com.fluentvalidator.builder.WheneverCollection;
import br.com.fluentvalidator.builder.WithValidator;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.exception.ValidationException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

/**
 * Implementation of RuleBuilderCollection for building validation rules on collection properties.
 * Provides fluent API for configuring validation rules on collections.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of elements in the collection
 */
public class RuleBuilderCollectionImpl<T, P> extends AbstractRuleBuilder<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> implements RuleBuilderCollection<T, P>, WhenCollection<T, P>, WheneverCollection<T, P> {

  private final Collection<Rule<Collection<P>>> rules = new LinkedList<>();

  private final RuleProcessorStrategy ruleProcessor = RuleProcessorStrategy.getFailFast();

  private ValidationRule<P, Collection<P>> currentValidation;

  /**
   * Constructs a new RuleBuilderCollectionImpl with a field name and function.
   *
   * @param fieldName the field name to use in error messages
   * @param function the function to extract the collection property
   */
  public RuleBuilderCollectionImpl(final String fieldName, final Function<T, Collection<P>> function) {
    super(fieldName, function);
  }

  /**
   * Constructs a new RuleBuilderCollectionImpl with only a function.
   *
   * @param function the function to extract the collection property
   */
  public RuleBuilderCollectionImpl(final Function<T, Collection<P>> function) {
    super(function);
  }

  /**
   * Applies all validation rules to the collection property of the instance.
   *
   * @param instance the instance to validate
   * @return true if all validations pass, false otherwise
   */
  @Override
  public boolean apply(final T instance) {
    final Collection<P> value = Objects.nonNull(instance) ? function.apply(instance) : null;
    return ruleProcessor.process(instance, value, rules);
  }

  /**
   * Sets the whenever condition for the current validation rule.
   *
   * @param whenever the predicate that determines when to apply the rule
   * @return the WheneverCollection builder for chaining
   */
  @Override
  public WheneverCollection<T, P> whenever(final Predicate<Collection<P>> whenever) {
    currentValidation = new ValidatorRuleInternal(fieldName, whenever);
    rules.add(currentValidation);
    return this;
  }

  /**
   * Sets the must predicate for the current validation rule.
   *
   * @param must the predicate that must be satisfied
   * @return the Must builder for chaining
   */
  @Override
  public Must<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> must(final Predicate<Collection<P>> must) {
    currentValidation = new ValidationRuleInternal(fieldName, must);
    rules.add(currentValidation);
    return this;
  }

  /**
   * Sets the error message for the current validation rule using a static string.
   *
   * @param message the error message
   * @return the Message builder for chaining
   */
  @Override
  public Message<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withMessage(final String message) {
    currentValidation.withMessage(obj -> message);
    return this;
  }

  /**
   * Sets the error message for the current validation rule using a function.
   *
   * @param message the function to generate the error message
   * @return the Message builder for chaining
   */
  @Override
  public Message<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withMessage(final Function<T, String> message) {
    currentValidation.withMessage(message);
    return this;
  }

  /**
   * Sets the error code for the current validation rule using a static string.
   *
   * @param code the error code
   * @return the Code builder for chaining
   */
  @Override
  public Code<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withCode(final String code) {
    currentValidation.withCode(obj -> code);
    return this;
  }

  /**
   * Sets the error code for the current validation rule using a function.
   *
   * @param code the function to generate the error code
   * @return the Code builder for chaining
   */
  @Override
  public Code<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withCode(final Function<T, String> code) {
    currentValidation.withCode(code);
    return this;
  }

  /**
   * Sets the field name for the current validation rule using a static string.
   *
   * @param fieldName the field name
   * @return the FieldName builder for chaining
   */
  @Override
  public FieldName<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withFieldName(final String fieldName) {
    currentValidation.withFieldName(obj -> fieldName);
    return this;
  }

  /**
   * Sets the field name for the current validation rule using a function.
   *
   * @param fieldName the function to generate the field name
   * @return the FieldName builder for chaining
   */
  @Override
  public FieldName<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withFieldName(final Function<T, String> fieldName) {
    currentValidation.withFieldName(fieldName);
    return this;
  }

  /**
   * Sets the attempted value for the current validation rule using a static value.
   *
   * @param attemptedValue the attempted value
   * @return the AttemptedValue builder for chaining
   */
  @Override
  public AttemptedValue<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withAttempedValue(final Object attemptedValue) {
    currentValidation.withAttemptedValue(obj -> attemptedValue);
    return this;
  }

  /**
   * Sets the attempted value for the current validation rule using a function.
   *
   * @param attemptedValue the function to generate the attempted value
   * @return the AttemptedValue builder for chaining
   */
  @Override
  public AttemptedValue<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withAttempedValue(final Function<T, Object> attemptedValue) {
    currentValidation.withAttemptedValue(attemptedValue);
    return this;
  }

  /**
   * Sets a custom handler for invalid field scenarios.
   *
   * @param handlerInvalidField the handler to process invalid field events
   * @return the HandleInvalidField builder for chaining
   */
  @Override
  public HandleInvalidField<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> handlerInvalidField(final HandlerInvalidField<Collection<P>> handlerInvalidField) {
    currentValidation.withHandlerInvalidField(handlerInvalidField);
    return this;
  }

  /**
   * Marks the current validation rule as critical.
   *
   * @return the Critical builder for chaining
   */
  @Override
  public Critical<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> critical() {
    currentValidation.critical();
    return this;
  }

  /**
   * Marks the current validation rule as critical with a custom exception class.
   *
   * @param clazz the custom ValidationException class
   * @return the Critical builder for chaining
   */
  @Override
  public Critical<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> critical(final Class<? extends ValidationException> clazz) {
    currentValidation.critical(clazz);
    return this;
  }

  /**
   * Associates a validator to be applied to each element in the collection.
   *
   * @param validator the validator to apply
   * @return the WithValidator builder for chaining
   */
  @Override
  public WithValidator<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withValidator(final Validator<P> validator) {
    currentValidation.withValidator(validator);
    return this;
  }

  /**
   * Sets the when condition for the current validation rule.
   *
   * @param when the predicate that must be true for validation to proceed
   * @return the WhenCollection builder for chaining
   */
  @Override
  public WhenCollection<T, P> when(final Predicate<Collection<P>> when) {
    currentValidation.when(when);
    return this;
  }

  /**
   * Internal class for validation rules that use a must predicate.
   */
  class ValidationRuleInternal extends AbstractValidationRule<P, Collection<P>> {

    /**
     * Constructs a new ValidationRuleInternal.
     *
     * @param fieldName the field name function
     * @param must the must predicate
     */
    ValidationRuleInternal(final Function<T, String> fieldName, final Predicate<Collection<P>> must) {
      super.must(must);
      super.withFieldName(fieldName);
    }

    @Override
    public boolean support(final Collection<P> instance) {
      return Boolean.TRUE.equals(getWhen().test(instance));
    }

    @Override
    public boolean apply(final Object obj, final Collection<P> instance) {

      final boolean apply = getMust().test(instance);

      if (Boolean.FALSE.equals(apply)) {
        ValidationContext.get().addErrors(getHandlerInvalid().handle(obj, instance));
      }

      if (Objects.nonNull(getCriticalException()) && Boolean.FALSE.equals(apply)) {
        throw ValidationException.create(getCriticalException());
      }

      return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));

    }

  }

  /**
   * Internal class for validation rules that use a whenever predicate.
   */
  class ValidatorRuleInternal extends AbstractValidationRule<P, Collection<P>> {

    /**
     * Constructs a new ValidatorRuleInternal.
     *
     * @param fieldName the field name function
     * @param whenever the whenever predicate
     */
    ValidatorRuleInternal(final Function<T, String> fieldName, final Predicate<Collection<P>> whenever) {
      super.whenever(whenever);
      super.withFieldName(fieldName);
    }

    @Override
    public boolean support(final Collection<P> instance) {
      return Boolean.TRUE.equals(getWhenever().test(instance));
    }

    @Override
    public boolean apply(final Object obj, final Collection<P> instance) {

      final boolean apply = ruleProcessor.process(obj, instance, getValidator());

      if (Objects.nonNull(getCriticalException()) && Boolean.FALSE.equals(apply)) {
        throw ValidationException.create(getCriticalException());
      }

      return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
    }

  }

}
