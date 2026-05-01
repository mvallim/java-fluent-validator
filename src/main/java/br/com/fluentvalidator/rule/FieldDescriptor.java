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
 * Interface for describing field information used in validation error reporting.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 */
interface FieldDescriptor<T, P> {

  /**
   * Returns the error message for the given instance.
   *
   * @param instance the instance to get the message for
   * @return the error message
   */
  String getMessage(final T instance);

  /**
   * Returns the error code for the given instance.
   *
   * @param instance the instance to get the code for
   * @return the error code
   */
  String getCode(final T instance);

  /**
   * Returns the field name for the given instance.
   *
   * @param instance the instance to get the field name for
   * @return the field name
   */
  String getFieldName(final T instance);

  /**
   * Returns the attempted value for the given instance.
   *
   * @param instance the instance to get the attempted value for
   * @param defaultValue the default value to return if no attempted value is set
   * @return the attempted value, or defaultValue if not set
   */
  Object getAttemptedValue(final T instance, final P defaultValue);

}
