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
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.rule.Rule;
import br.com.fluentvalidator.transform.ValidationResultTransform;

/**
 * Interface that defines the contract for fluent validators.
 * <p>
 * This interface extends {@link Rule} and provides methods for building and executing
 * validation rules in a fluent, chainable manner. It supports validation of single objects,
 * collections of objects, and provides flexible result transformation capabilities.
 * </p>
 * <p>
 * Implementations should define validation rules by implementing the {@link #rules()} method
 * and using the fluent API provided by {@link #ruleFor(Function)} and {@link #ruleForEach(Function)}
 * to build validation constraints.
 * </p>
 * <p>
 * This interface supports:
 * <ul>
 *   <li>Single object validation with {@link #validate(Object)}</li>
 *   <li>Collection validation with {@link #validate(Collection)}</li>
 *   <li>Result transformation for custom output formats</li>
 *   <li>Fail-fast validation mode</li>
 *   <li>Contextual property storage and retrieval</li>
 * </ul>
 * </p>
 *
 * @param <T> the type of object being validated
 */
public interface Validator<T> extends Rule<T> {

  /**
   * Defines the validation rules for this validator.
   * <p>
   * Subclasses should implement this method to define validation rules using
   * the fluent API provided by {@link #ruleFor(Function)} and {@link #ruleForEach(Function)}.
   * This method is called once during initialization in a thread-safe manner.
   * </p>
   */
  void rules();

  /**
   * Configures this validator to use fail-fast rule processing.
   * <p>
   * When fail-fast mode is enabled, validation will stop at the first rule failure
   * instead of continuing to evaluate all rules. This can improve performance when
   * early validation failure is acceptable.
   * </p>
   */
  void failFastRule();

  /**
   * Returns the current validation counter.
   * <p>
   * The counter tracks the number of validation operations or rules that have been
   * processed. This can be useful for debugging, monitoring, or performance analysis.
   * </p>
   *
   * @return the current counter value, or null if no counter is available
   */
  Integer getCounter();

  /**
   * Sets a property name to be used in the validation context.
   * <p>
   * This property name will be associated with the validated object in the validation
   * context, allowing rules to access contextual information during validation.
   * </p>
   *
   * @param property the property name to set in the validation context
   */
  void setPropertyOnContext(final String property);

  /**
   * Retrieves a property value from the validation context.
   * <p>
   * This method allows access to contextual information that was previously stored
   * in the validation context, enabling rules to make decisions based on broader
   * validation state.
   * </p>
   *
   * @param <P> the type of the property value
   * @param property the name of the property to retrieve
   * @param clazz the class type of the property value
   * @return the property value cast to the specified type, or null if not found
   */
  <P> P getPropertyOnContext(final String property, final Class<P> clazz);

  /**
   * Validates a single instance and returns the validation result.
   * <p>
   * This method processes the given instance through all configured validation rules
   * and returns a comprehensive result containing any validation errors or success indicators.
   * </p>
   *
   * @param instance the object instance to validate
   * @return a ValidationResult containing the outcome of validation
   */
  ValidationResult validate(final T instance);

  /**
   * Validates a single instance and transforms the result using the provided transformer.
   * <p>
   * This method combines validation with result transformation in a single operation,
   * allowing for custom result formats or processing without intermediate objects.
   * </p>
   *
   * @param <E> the type of the transformed result
   * @param instance the object instance to validate
   * @param transform the transformer to apply to the validation result
   * @return the transformed validation result
   */
  <E> E validate(final T instance, final ValidationResultTransform<E> transform);

  /**
   * Validates a collection of instances and returns a list of validation results.
   * <p>
   * Each instance in the collection is validated independently, and the results are
   * collected into a list. The order of results corresponds to the order of instances
   * in the input collection.
   * </p>
   *
   * @param instances the collection of instances to validate
   * @return a list of ValidationResult objects, one for each input instance
   */
  List<ValidationResult> validate(final Collection<T> instances);

  /**
   * Validates a collection of instances and transforms each result using the provided transformer.
   * <p>
   * This method combines collection validation with result transformation, applying
   * the transformer to each individual validation result.
   * </p>
   *
   * @param <E> the type of the transformed results
   * @param instances the collection of instances to validate
   * @param transform the transformer to apply to each validation result
   * @return a list of transformed validation results
   */
  <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> transform);

  /**
   * Creates a validation rule for a specific property of the validated object.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply to a
   * property extracted from the validated object using the provided function. The property
   * name will be automatically derived from the function if possible.
   * </p>
   *
   * @param <P> the type of the property being validated
   * @param function a function that extracts the property value from the validated object
   * @return a RuleBuilderProperty for chaining additional validation constraints
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function);

  /**
   * Creates a validation rule for a named property of the validated object.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply to a
   * property extracted from the validated object. The field name is explicitly provided
   * and will be used in error messages and validation context.
   * </p>
   *
   * @param <P> the type of the property being validated
   * @param fieldName the name of the field being validated (used in error messages)
   * @param function a function that extracts the property value from the validated object
   * @return a RuleBuilderProperty for chaining additional validation constraints
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final String fieldName, final Function<T, P> function);

  /**
   * Creates validation rules for each element in a collection property.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply to each
   * element of a collection extracted from the validated object. The field name will be
   * automatically derived from the function if possible.
   * </p>
   *
   * @param <P> the type of elements in the collection being validated
   * @param function a function that extracts the collection from the validated object
   * @return a RuleBuilderCollection for chaining additional validation constraints
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function);

  /**
   * Creates validation rules for each element in a collection property.
   * <p>
   * This method starts a fluent chain for defining validation rules that apply to each
   * element of a collection extracted from the validated object. The field name is
   * explicitly provided for error reporting.
   * </p>
   *
   * @param <P> the type of elements in the collection being validated
   * @param fieldName the name of the collection field being validated
   * @param function a function that extracts the collection from the validated object
   * @return a RuleBuilderCollection for chaining additional validation constraints
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final String fieldName, final Function<T, Collection<P>> function);
}
