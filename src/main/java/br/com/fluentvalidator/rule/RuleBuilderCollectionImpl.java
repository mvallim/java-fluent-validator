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
 * Implementation of {@link RuleBuilderCollection} for building validation rules on a collection property of an object.
 * This class supports fluent configuration of validation rules for each element in the collection,
 * including conditions, messages, codes, and nested validators.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of elements in the collection property
 */
public class RuleBuilderCollectionImpl<T, P> extends AbstractRuleBuilder<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> implements RuleBuilderCollection<T, P>, WhenCollection<T, P>, WheneverCollection<T, P> {

  private final Collection<Rule<Collection<P>>> rules = new LinkedList<>();

  private final RuleProcessorStrategy ruleProcessor = RuleProcessorStrategy.getFailFast();

  private ValidationRule<P, Collection<P>> currentValidation;

  /**
   * Constructs a new RuleBuilderCollectionImpl with a static field name and property extractor function.
   *
   * @param fieldName the static field name for error reporting
   * @param function the function to extract the collection property from the object being validated
   */
  public RuleBuilderCollectionImpl(final String fieldName, final Function<T, Collection<P>> function) {
    super(fieldName, function);
  }

  /**
   * Constructs a new RuleBuilderCollectionImpl with only a property extractor function (no static field name).
   *
   * @param function the function to extract the collection property from the object being validated
   */
  public RuleBuilderCollectionImpl(final Function<T, Collection<P>> function) {
    super(function);
  }

  /**
   * Applies all configured validation rules to the collection property extracted from the given instance.
   *
   * @param instance the object to validate
   * @return {@code true} if all rules pass, {@code false} otherwise
   */
  @Override
  public boolean apply(final T instance) {
    final Collection<P> value = Objects.nonNull(instance) ? function.apply(instance) : null;
    return ruleProcessor.process(instance, value, rules);
  }

  /**
   * Adds a new rule that is active when the given predicate returns true, for nested validation of the collection.
   *
   * @param whenever the predicate to check for rule activation
   * @return this WheneverCollection instance for fluent chaining
   */
  @Override
  public WheneverCollection<T, P> whenever(final Predicate<Collection<P>> whenever) {
    this.currentValidation = new ValidatorRuleInternal(fieldName, whenever);
    this.rules.add(this.currentValidation);
    return this;
  }

  /**
   * Adds a new validation rule with the given predicate for the collection.
   *
   * @param must the validation predicate
   * @return this Must instance for fluent chaining
   */
  @Override
  public Must<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> must(final Predicate<Collection<P>> must) {
    this.currentValidation = new ValidationRuleInternal(fieldName, must);
    this.rules.add(this.currentValidation);
    return this;
  }

  /**
   * Sets a static error message for the current validation rule.
   *
   * @param message the static error message
   * @return this Message instance for fluent chaining
   */
  @Override
  public Message<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withMessage(final String message) {
    this.currentValidation.withMessage(obj -> message);
    return this;
  }

  /**
   * Sets a dynamic error message function for the current validation rule.
   *
   * @param message the function to generate the error message
   * @return this Message instance for fluent chaining
   */
  @Override
  public Message<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withMessage(final Function<T, String> message) {
    this.currentValidation.withMessage(message);
    return this;
  }

  /**
   * Sets a static error code for the current validation rule.
   *
   * @param code the static error code
   * @return this Code instance for fluent chaining
   */
  @Override
  public Code<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withCode(final String code) {
    this.currentValidation.withCode(obj -> code);
    return this;
  }

  /**
   * Sets a dynamic error code function for the current validation rule.
   *
   * @param code the function to generate the error code
   * @return this Code instance for fluent chaining
   */
  @Override
  public Code<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withCode(final Function<T, String> code) {
    this.currentValidation.withCode(code);
    return this;
  }

  /**
   * Sets a static field name for the current validation rule.
   *
   * @param fieldName the static field name
   * @return this FieldName instance for fluent chaining
   */
  @Override
  public FieldName<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withFieldName(final String fieldName) {
    this.currentValidation.withFieldName(obj -> fieldName);
    return this;
  }

  /**
   * Sets a dynamic field name function for the current validation rule.
   *
   * @param fieldName the function to generate the field name
   * @return this FieldName instance for fluent chaining
   */
  @Override
  public FieldName<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withFieldName(final Function<T, String> fieldName) {
    this.currentValidation.withFieldName(fieldName);
    return this;
  }

  /**
   * Sets a static attempted value for the current validation rule.
   *
   * @param attemptedValue the static attempted value
   * @return this AttemptedValue instance for fluent chaining
   */
  @Override
  public AttemptedValue<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withAttempedValue(final Object attemptedValue) {
    this.currentValidation.withAttemptedValue(obj -> attemptedValue);
    return this;
  }

  /**
   * Sets a dynamic attempted value function for the current validation rule.
   *
   * @param attemptedValue the function to generate the attempted value
   * @return this AttemptedValue instance for fluent chaining
   */
  @Override
  public AttemptedValue<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withAttempedValue(final Function<T, Object> attemptedValue) {
    this.currentValidation.withAttemptedValue(attemptedValue);
    return this;
  }

  /**
   * Sets a handler for invalid fields for the current validation rule.
   *
   * @param handlerInvalidField the handler for invalid fields
   * @return this HandleInvalidField instance for fluent chaining
   */
  @Override
  public HandleInvalidField<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> handlerInvalidField(final HandlerInvalidField<Collection<P>> handlerInvalidField) {
    this.currentValidation.withHandlerInvalidField(handlerInvalidField);
    return this;
  }

  /**
   * Marks the current validation rule as critical, throwing a default {@link ValidationException} on failure.
   *
   * @return this Critical instance for fluent chaining
   */
  @Override
  public Critical<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> critical() {
    this.currentValidation.critical();
    return this;
  }

  /**
   * Marks the current validation rule as critical, throwing the specified exception on failure.
   *
   * @param clazz the type of exception to throw on failure
   * @return this Critical instance for fluent chaining
   */
  @Override
  public Critical<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> critical(final Class<? extends ValidationException> clazz) {
    this.currentValidation.critical(clazz);
    return this;
  }

  /**
   * Associates a nested validator with the current validation rule for validating the collection property.
   *
   * @param validator the nested validator to apply
   * @return this WithValidator instance for fluent chaining
   */
  @Override
  public WithValidator<T, Collection<P>, WhenCollection<T, P>, WheneverCollection<T, P>> withValidator(final Validator<P> validator) {
    this.currentValidation.withValidator(validator);
    return this;
  }

  /**
   * Sets the predicate that determines when the current validation rule should be applied.
   *
   * @param when the predicate to check before applying the rule
   * @return this WhenCollection instance for fluent chaining
   */
  @Override
  public WhenCollection<T, P> when(final Predicate<Collection<P>> when) {
    this.currentValidation.when(when);
    return this;
  }

  /**
   * Internal implementation of {@link ValidationRule} for collection validation rules (must predicates).
   */
  class ValidationRuleInternal extends AbstractValidationRule<P, Collection<P>> {

    /**
     * Constructs a new ValidationRuleInternal with the given field name function and must predicate.
     *
     * @param fieldName the function to generate the field name
     * @param must the validation predicate
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
   * Internal implementation of {@link ValidationRule} for nested validator rules on collections (whenever predicates).
   */
  class ValidatorRuleInternal extends AbstractValidationRule<P, Collection<P>> {

    /**
     * Constructs a new ValidatorRuleInternal with the given field name function and whenever predicate.
     *
     * @param fieldName the function to generate the field name
     * @param whenever the predicate to check for rule activation
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
