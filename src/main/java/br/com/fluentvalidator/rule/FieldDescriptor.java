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

/**
 * Describes the fields associated with a validation rule, providing access to error message, code,
 * field name, and attempted value for error reporting.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 */
interface FieldDescriptor<T, P> {

  /**
   * Returns the error message for this rule, based on the given instance.
   *
   * @param instance the object being validated
   * @return the error message, or {@code null} if not set
   */
  String getMessage(final T instance);

  /**
   * Returns the error code for this rule, based on the given instance.
   *
   * @param instance the object being validated
   * @return the error code, or {@code null} if not set
   */
  String getCode(final T instance);

  /**
   * Returns the field name associated with this rule, based on the given instance.
   *
   * @param instance the object being validated
   * @return the field name, or {@code null} if not set
   */
  String getFieldName(final T instance);

  /**
   * Returns the attempted value (the value that failed validation) for error reporting.
   *
   * @param instance the object being validated
   * @param defaultValue the default value to return if no attempted value is set
   * @return the attempted value, or {@code defaultValue} if not set
   */
  Object getAttemptedValue(final T instance, final P defaultValue);

}
