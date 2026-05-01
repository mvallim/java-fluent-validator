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

package br.com.fluentvalidator;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.context.ProcessorContext;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationContext.Context;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.rule.Rule;
import br.com.fluentvalidator.rule.RuleBuilderCollectionImpl;
import br.com.fluentvalidator.rule.RuleBuilderPropertyImpl;
import br.com.fluentvalidator.rule.RuleProcessorStrategy;
import br.com.fluentvalidator.transform.ValidationResultTransform;

/**
 * Abstract base class for implementing validators.
 * Provides the core validation logic and rule management.
 *
 * @param <T> the type of object this validator validates
 */
public abstract class AbstractValidator<T> implements Validator<T> {

  private final List<Rule<T>> rules = new LinkedList<>();

  private final Initializer<T> initialize;

  private String property;

  private RuleProcessorStrategy ruleProcessor = RuleProcessorStrategy.getDefault();

  /**
   * Inner class responsible for lazily initializing the validation rules.
   * Uses double-checked locking to ensure thread-safe initialization.
   *
   * @param <T> the type of object being validated
   */
  private static class Initializer<T> {

    private final AtomicReference<Boolean> atomicReference = new AtomicReference<>(Boolean.FALSE);

    private final Validator<T> validator;

    Initializer(final Validator<T> validator) {
      this.validator = validator;
    }

    /**
     * Initializes the validation rules by calling the rules() method exactly once.
     */
    public void init() {
      if (isNotInitialized()) {
        synchronized (atomicReference) {
          if (isNotInitialized()) { // double check if was initialized
            validator.rules();
            final Boolean oldValue = atomicReference.get();
            final Boolean newValue = Boolean.TRUE;
            atomicReference.compareAndSet(oldValue, newValue);
          }
        }
      }
    }

    private boolean isNotInitialized() {
      return Boolean.FALSE.equals(atomicReference.get());
    }

  }

  /**
   * Constructs a new AbstractValidator and sets up the lazy initializer.
   */
  protected AbstractValidator() {
    initialize = new Initializer<>(this);
  }

  /**
   * Configures the validator to use fail-fast mode.
   * In fail-fast mode, validation stops on the first failure.
   */
  @Override
  public void failFastRule() {
    ruleProcessor = RuleProcessorStrategy.getFailFast();
  }

  /**
   * Returns the number of validations performed in the current processor context.
   *
   * @return the number of validations performed
   */
  @Override
  public Integer getCounter() {
    return ProcessorContext.get().get();
  }

  /**
   * Sets a property on the validation context for use during validation.
   *
   * @param property the name of the property to set
   */
  @Override
  public void setPropertyOnContext(final String property) {
    this.property = property;
  }

  /**
   * Retrieves a property from the validation context.
   *
   * @param property the name of the property to retrieve
   * @param clazz the expected class of the property value
   * @param <P> the type of the property value
   * @return the property value, or null if not found
   */
  @Override
  public <P> P getPropertyOnContext(final String property, final Class<P> clazz) {
    return ValidationContext.get().getProperty(property, clazz);
  }

  /**
   * Validates a single instance and returns the validation result.
   *
   * @param instance the instance to validate
   * @return the validation result containing any errors
   */
  @Override
  public ValidationResult validate(final T instance) {
    try (final Context context = ValidationContext.get()) {
      ruleProcessor.process(instance, this);
      return context.getValidationResult();
    }
  }

  /**
   * Validates a single instance and transforms the result.
   *
   * @param instance the instance to validate
   * @param resultTransform the transform to apply to the validation result
   * @param <E> the type of the transformed result
   * @return the transformed validation result
   */
  @Override
  public <E> E validate(final T instance, final ValidationResultTransform<E> resultTransform) {
    return resultTransform.transform(validate(instance));
  }

  /**
   * Validates a collection of instances and returns a list of validation results.
   *
   * @param instances the collection of instances to validate
   * @return an unmodifiable list of validation results, one per instance
   */
  @Override
  public List<ValidationResult> validate(final Collection<T> instances) {
    return Collections.unmodifiableList(instances.stream().map(this::validate).collect(Collectors.toList()));
  }

  /**
   * Validates a collection of instances and transforms each result.
   *
   * @param instances the collection of instances to validate
   * @param resultTransform the transform to apply to each validation result
   * @param <E> the type of the transformed result
   * @return an unmodifiable list of transformed validation results
   */
  @Override
  public <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> resultTransform) {
    return Collections.unmodifiableList(instances.stream().map(instance -> this.validate(instance, resultTransform)).collect(Collectors.toList()));
  }

  /**
   * Applies all validation rules to the given instance.
   * This method initializes the rules if not already done and sets the context property.
   *
   * @param instance the instance to validate
   * @return true if all validations pass, false otherwise
   */
  @Override
  public boolean apply(final T instance) {
    initialize.init();
    ValidationContext.get().setProperty(property, instance);
    return ruleProcessor.process(instance, instance, rules);
  }

  /**
   * Creates a rule builder for validating a property of the object.
   *
   * @param function the function to extract the property value
   * @param <P> the type of the property
   * @return a rule builder for defining validation rules on the property
   */
  @Override
  public <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function) {
    final RuleBuilderPropertyImpl<T, P> rule = new RuleBuilderPropertyImpl<>(function);
    rules.add(rule);
    return rule;
  }

  /**
   * Creates a rule builder for validating a property with a custom field name.
   *
   * @param fieldName the name to use in validation error messages
   * @param function the function to extract the property value
   * @param <P> the type of the property
   * @return a rule builder for defining validation rules on the property
   */
  @Override
  public <P> RuleBuilderProperty<T, P> ruleFor(final String fieldName, final Function<T, P> function) {
    final RuleBuilderPropertyImpl<T, P> rule = new RuleBuilderPropertyImpl<>(fieldName, function);
    rules.add(rule);
    return rule;
  }

  /**
   * Creates a rule builder for validating each element in a collection property with a custom field name.
   *
   * @param fieldName the name to use in validation error messages
   * @param function the function to extract the collection property
   * @param <P> the type of elements in the collection
   * @return a rule builder for defining validation rules on the collection
   */
  @Override
  public <P> RuleBuilderCollection<T, P> ruleForEach(final String fieldName, final Function<T, Collection<P>> function) {
    final RuleBuilderCollectionImpl<T, P> rule = new RuleBuilderCollectionImpl<>(fieldName, function);
    rules.add(rule);
    return rule;
  }

  /**
   * Creates a rule builder for validating each element in a collection property.
   *
   * @param function the function to extract the collection property
   * @param <P> the type of elements in the collection
   * @return a rule builder for defining validation rules on the collection
   */
  @Override
  public <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function) {
    final RuleBuilderCollectionImpl<T, P> rule = new RuleBuilderCollectionImpl<>(function);
    rules.add(rule);
    return rule;
  }

}
