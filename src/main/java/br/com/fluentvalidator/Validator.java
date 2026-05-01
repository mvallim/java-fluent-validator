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
 * Main interface for defining validators that can validate objects of type T.
 * Validators are used to define validation rules and apply them to objects.
 *
 * @param <T> the type of object this validator can validate
 */
public interface Validator<T> extends Rule<T> {

  /**
   * Defines the validation rules for this validator.
   * This method is called once to initialize the validation rules.
   */
  void rules();

  /**
   * Configures the validator to use fail-fast mode.
   * In fail-fast mode, validation stops on the first failure.
   */
  void failFastRule();

  /**
   * Returns the number of validations performed in the current context.
   *
   * @return the number of validations performed
   */
  Integer getCounter();

  /**
   * Sets a property on the validation context for use during validation.
   *
   * @param property the name of the property to set
   */
  void setPropertyOnContext(final String property);

  /**
   * Retrieves a property from the validation context.
   *
   * @param property the name of the property to retrieve
   * @param clazz the expected class of the property value
   * @param <P> the type of the property value
   * @return the property value, or null if not found
   */
  <P> P getPropertyOnContext(final String property, final Class<P> clazz);

  /**
   * Validates a single instance and returns the validation result.
   *
   * @param instance the instance to validate
   * @return the validation result containing any errors
   */
  ValidationResult validate(final T instance);

  /**
   * Validates a single instance and transforms the result using the provided transform.
   *
   * @param instance the instance to validate
   * @param transform the transform to apply to the validation result
   * @param <E> the type of the transformed result
   * @return the transformed validation result
   */
  <E> E validate(final T instance, final ValidationResultTransform<E> transform);

  /**
   * Validates a collection of instances and returns a list of validation results.
   *
   * @param instances the collection of instances to validate
   * @return an unmodifiable list of validation results, one per instance
   */
  List<ValidationResult> validate(final Collection<T> instances);

  /**
   * Validates a collection of instances and transforms each result.
   *
   * @param instances the collection of instances to validate
   * @param transform the transform to apply to each validation result
   * @param <E> the type of the transformed result
   * @return an unmodifiable list of transformed validation results
   */
  <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> transform);

  /**
   * Creates a rule builder for validating a property of the object.
   *
   * @param function the function to extract the property value
   * @param <P> the type of the property
   * @return a rule builder for defining validation rules on the property
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function);

  /**
   * Creates a rule builder for validating a property with a custom field name.
   *
   * @param fieldName the name to use in validation error messages
   * @param function the function to extract the property value
   * @param <P> the type of the property
   * @return a rule builder for defining validation rules on the property
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final String fieldName, final Function<T, P> function);

  /**
   * Creates a rule builder for validating each element in a collection property.
   *
   * @param function the function to extract the collection property
   * @param <P> the type of elements in the collection
   * @return a rule builder for defining validation rules on the collection
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function);

  /**
   * Creates a rule builder for validating each element in a collection property with a custom field name.
   *
   * @param fieldName the name to use in validation error messages
   * @param function the function to extract the collection property
   * @param <P> the type of elements in the collection
   * @return a rule builder for defining validation rules on the collection
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final String fieldName, final Function<T, Collection<P>> function);

}
