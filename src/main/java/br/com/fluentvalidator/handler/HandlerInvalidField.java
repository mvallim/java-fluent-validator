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

package br.com.fluentvalidator.handler;

import java.util.Collection;
import java.util.Collections;
import br.com.fluentvalidator.context.Error;

/**
 * Interface for handling invalid field validation scenarios.
 * <p>
 * This interface provides a contract for implementing custom handlers that process
 * validation failures for specific field types. Implementations can define custom
 * logic to generate appropriate error messages or perform additional processing
 * when field validation fails.
 * </p>
 * <p>
 * The interface supports two handling approaches:
 * <ul>
 *   <li>Simple handling based only on the attempted value</li>
 *   <li>Context-aware handling that considers both the object instance and attempted value</li>
 * </ul>
 * </p>
 *
 * @param <P> the type of the field value being validated
 */
public interface HandlerInvalidField<P> {

  /**
   * Handles validation failure for a field with the given attempted value.
   * <p>
   * This method is called when field validation fails and provides an opportunity
   * to generate custom error messages or perform additional processing based on
   * the attempted value.
   * </p>
   * <p>
   * The default implementation returns an empty collection, indicating no errors
   * should be added beyond the standard validation failure.
   * </p>
   *
   * @param attemptedValue the value that failed validation, may be {@code null}
   * @return a collection of {@link Error} objects representing validation errors,
   *         never {@code null} but may be empty
   */
  default Collection<Error> handle(final P attemptedValue) {
    return Collections.emptyList();
  }

  /**
   * Handles validation failure for a field with context of the containing object instance.
   * <p>
   * This method provides additional context by including the object instance that
   * contains the field being validated. This allows for more sophisticated error
   * handling that can consider the state of the entire object.
   * </p>
   * <p>
   * The default implementation delegates to {@link #handle(Object)} with only
   * the attempted value, ignoring the instance context.
   * </p>
   *
   * @param instance the object instance containing the field being validated,
   *                 may be {@code null}
   * @param attemptedValue the value that failed validation, may be {@code null}
   * @return a collection of {@link Error} objects representing validation errors,
   *         never {@code null} but may be empty
   */
  default Collection<Error> handle(final Object instance, final P attemptedValue) {
    return handle(attemptedValue);
  }

}
