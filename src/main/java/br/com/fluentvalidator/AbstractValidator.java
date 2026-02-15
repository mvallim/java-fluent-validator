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
 * Abstract base class for implementing fluent validators.
 * <p>
 * This class provides the core functionality for building and executing validation rules
 * in a fluent, chainable manner. It supports both single object and collection validation,
 * with configurable processing strategies including fail-fast behavior.
 * </p>
 * <p>
 * Subclasses should implement the {@link Validator#rules()} method to define
 * validation rules using the fluent API provided by {@link #ruleFor(Function)} and
 * {@link #ruleForEach(Function)} methods.
 * </p>
 * <p>
 * Thread Safety: This class is thread-safe for validation operations. Rule initialization
 * is performed using double-checked locking with atomic references to prevent race conditions.
 * </p>
 *
 * @param <T> the type of object being validated
 */
public abstract class AbstractValidator<T> implements Validator<T> {

  /**
   * List of validation rules to be applied to instances of type T.
   * Rules are executed in the order they were added.
   */
  private final List<Rule<T>> rules = new LinkedList<>();

  /**
   * Initializer responsible for thread-safe rule initialization.
   */
  private final Initializer<T> initialize;

  /**
   * Property name to be set in the validation context during validation.
   */
  private String property;

  /**
   * Strategy for processing validation rules. Defaults to standard processing,
   * but can be changed to fail-fast mode.
   */
  private RuleProcessorStrategy ruleProcessor = RuleProcessorStrategy.getDefault();

  /**
   * Thread-safe initializer for validation rules.
   * <p>
   * This inner class ensures that validation rules are initialized exactly once
   * per validator instance, even in multi-threaded environments. It uses
   * Compare-And-Swap (CAS) operations with double-checked locking to prevent
   * race conditions during initialization.
   * </p>
   *
   * @param <T> the type of object being validated
   */
  private static class Initializer<T> {
    /**
     * Atomic reference to track initialization state.
     * FALSE indicates not initialized, TRUE indicates initialized.
     */
    private final AtomicReference<Boolean> atomicReference = new AtomicReference<>(Boolean.FALSE);

    /**
     * Reference to the validator instance being initialized.
     */
    private final Validator<T> validator;

    /**
     * Constructs a new initializer for the given validator.
     *
     * @param validator the validator instance to initialize
     */
    Initializer(final Validator<T> validator) {
      this.validator = validator;
    }

    /**
     * Initializes the validator rules in a thread-safe manner.
     * <p>
     * This method uses double-checked locking with atomic operations to ensure
     * that the validator's rules are initialized exactly once, even when called
     * concurrently from multiple threads. This prevents race conditions that
     * could occur during rule initialization.
     * </p>
     * <p>
     * The implementation follows the Compare-And-Swap (CAS) pattern for
     * lock-free programming where possible, falling back to synchronized
     * blocks only when necessary.
     * </p>
     *
     * @see <a href="https://en.wikipedia.org/wiki/Race_condition">Race Condition</a>
     * @see <a href="https://en.wikipedia.org/wiki/Compare-and-swap">Compare-and-swap</a>
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

    /**
     * Checks if the validator has not been initialized yet.
     *
     * @return true if the validator is not initialized, false otherwise
     */
    private boolean isNotInitialized() {
      return Boolean.FALSE.equals(atomicReference.get());
    }

  }

  /**
   * Constructs a new AbstractValidator instance.
   * <p>
   * Initializes the validator with default settings and creates
   * the thread-safe initializer for rule setup.
   * </p>
   */
  protected AbstractValidator() {
    this.initialize = new Initializer<>(this);
  }

  /**
   * Configures the validator to use fail-fast rule processing.
   * <p>
   * When fail-fast mode is enabled, validation will stop at the first
   * rule failure instead of continuing to evaluate all rules. This can
   * improve performance when early validation failure is acceptable.
   * </p>
   *
   * @see RuleProcessorStrategy#getFailFast()
   */
  @Override
  public void failFastRule() {
    this.ruleProcessor = RuleProcessorStrategy.getFailFast();
  }

  /**
   * Gets the current validation counter from the processor context.
   * <p>
   * The counter tracks the number of validation operations or rules
   * that have been processed. This can be useful for debugging,
   * monitoring, or performance analysis.
   * </p>
   *
   * @return the current counter value, or null if no counter is available
   */
  @Override
  public Integer getCounter() {
    return ProcessorContext.get().get();
  }

  /**
   * Sets a property name to be used in the validation context.
   * <p>
   * This property name will be associated with the validated object
   * in the validation context, allowing rules to access contextual
   * information during validation.
   * </p>
   *
   * @param property the property name to set in the validation context
   */
  @Override
  public void setPropertyOnContext(final String property) {
    this.property = property;
  }

  /**
   * Retrieves a property value from the validation context.
   * <p>
   * This method allows access to contextual information that was
   * previously stored in the validation context, enabling rules
   * to make decisions based on broader validation state.
   * </p>
   *
   * @param <P> the type of the property value
   * @param property the name of the property to retrieve
   * @param clazz the class type of the property value
   * @return the property value cast to the specified type, or null if not found
   */
  @Override
  public <P> P getPropertyOnContext(final String property, final Class<P> clazz) {
    return ValidationContext.get().getProperty(property, clazz);
  }

  /**
   * Validates a single instance and returns the validation result.
   * <p>
   * This method processes the given instance through all configured
   * validation rules and returns a comprehensive result containing
   * any validation errors or success indicators.
   * </p>
   *
   * @param instance the object instance to validate
   * @return a ValidationResult containing the outcome of validation
   * @throws IllegalArgumentException if the instance is null and null values are not supported
   */
  @Override
  public ValidationResult validate(final T instance) {
    try (final Context context = ValidationContext.get()) {
      ruleProcessor.process(instance, this);
      return context.getValidationResult();
    }
  }

  /**
   * Validates a single instance and transforms the result using the provided transformer.
   * <p>
   * This method combines validation with result transformation in a single operation,
   * allowing for custom result formats or processing without intermediate objects.
   * </p>
   *
   * @param <E> the type of the transformed result
   * @param instance the object instance to validate
   * @param resultTransform the transformer to apply to the validation result
   * @return the transformed validation result
   * @throws IllegalArgumentException if the instance is null and null values are not supported
   * @throws NullPointerException if resultTransform is null
   */
  @Override
  public <E> E validate(final T instance, final ValidationResultTransform<E> resultTransform) {
    return resultTransform.transform(validate(instance));
  }

  /**
   * Validates a collection of instances and returns a list of validation results.
   * <p>
   * Each instance in the collection is validated independently, and the results
   * are collected into an unmodifiable list. The order of results corresponds
   * to the order of instances in the input collection.
   * </p>
   *
   * @param instances the collection of instances to validate
   * @return an unmodifiable list of ValidationResult objects, one for each input instance
   * @throws NullPointerException if the instances collection is null
   */
  @Override
  public List<ValidationResult> validate(final Collection<T> instances) {
    return Collections.unmodifiableList(instances.stream().map(this::validate).collect(Collectors.toList()));
  }

  /**
   * Validates a collection of instances and transforms each result using the provided transformer.
   * <p>
   * This method combines collection validation with result transformation,
   * applying the transformer to each individual validation result.
   * </p>
   *
   * @param <E> the type of the transformed results
   * @param instances the collection of instances to validate
   * @param resultTransform the transformer to apply to each validation result
   * @return an unmodifiable list of transformed validation results
   * @throws NullPointerException if instances or resultTransform is null
   */
  @Override
  public <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> resultTransform) {
    return Collections.unmodifiableList(instances.stream().map(instance -> this.validate(instance, resultTransform)).collect(Collectors.toList()));
  }

  /**
   * Applies validation rules to an instance and returns a boolean result.
   * <p>
   * This method is typically called internally during the validation process.
   * It ensures rules are initialized, sets up the validation context, and
   * processes all rules against the given instance.
   * </p>
   *
   * @param instance the object instance to validate
   * @return true if all validation rules pass, false if any rule fails
   */
  @Override
  public boolean apply(final T instance) {
    this.initialize.init();
    ValidationContext.get().setProperty(this.property, instance);
    return ruleProcessor.process(instance, instance, rules);
  }

  /**
   * Creates a validation rule for a specific property of the validated object.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply
   * to a property extracted from the validated object using the provided function.
   * The property name will be automatically derived from the function if possible.
   * </p>
   *
   * @param <P> the type of the property being validated
   * @param function a function that extracts the property value from the validated object
   * @return a RuleBuilderProperty for chaining additional validation constraints
   * @throws NullPointerException if function is null
   */
  @Override
  public <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function) {
    final RuleBuilderPropertyImpl<T, P> rule = new RuleBuilderPropertyImpl<>(function);
    this.rules.add(rule);
    return rule;
  }

  /**
   * Creates a validation rule for a named property of the validated object.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply
   * to a property extracted from the validated object. The field name is explicitly
   * provided and will be used in error messages and validation context.
   * </p>
   *
   * @param <P> the type of the property being validated
   * @param fieldName the name of the field being validated (used in error messages)
   * @param function a function that extracts the property value from the validated object
   * @return a RuleBuilderProperty for chaining additional validation constraints
   * @throws NullPointerException if fieldName or function is null
   * @throws IllegalArgumentException if fieldName is empty
   */
  @Override
  public <P> RuleBuilderProperty<T, P> ruleFor(final String fieldName, final Function<T, P> function) {
    final RuleBuilderPropertyImpl<T, P> rule = new RuleBuilderPropertyImpl<>(fieldName, function);
    this.rules.add(rule);
    return rule;
  }

  /**
   * Creates validation rules for each element in a collection property.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply
   * to each element of a collection extracted from the validated object.
   * The field name is explicitly provided for error reporting.
   * </p>
   *
   * @param <P> the type of elements in the collection being validated
   * @param fieldName the name of the collection field being validated
   * @param function a function that extracts the collection from the validated object
   * @return a RuleBuilderCollection for chaining additional validation constraints
   * @throws NullPointerException if fieldName or function is null
   * @throws IllegalArgumentException if fieldName is empty
   */
  @Override
  public <P> RuleBuilderCollection<T, P> ruleForEach(final String fieldName, final Function<T, Collection<P>> function) {
    final RuleBuilderCollectionImpl<T, P> rule = new RuleBuilderCollectionImpl<>(fieldName, function);
    this.rules.add(rule);
    return rule;
  }

  /**
   * Creates validation rules for each element in a collection property.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply
   * to each element of a collection extracted from the validated object.
   * The field name will be automatically derived from the function if possible.
   * </p>
   *
   * @param <P> the type of elements in the collection being validated
   * @param function a function that extracts the collection from the validated object
   * @return a RuleBuilderCollection for chaining additional validation constraints
   * @throws NullPointerException if function is null
   */
  @Override
  public <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function) {
    final RuleBuilderCollectionImpl<T, P> rule = new RuleBuilderCollectionImpl<>(function);
    this.rules.add(rule);
    return rule;
  }

}
