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
 * Interface for handling invalid field scenarios.
 * Allows custom logic to be executed when a field fails validation.
 *
 * @param <P> the type of the property being validated
 */
public interface HandlerInvalidField<P> {

  /**
   * Handles an invalid field scenario.
   *
   * @param attemptedValue the value that failed validation
   * @return a collection of validation errors
   */
  default Collection<Error> handle(final P attemptedValue) {
    return Collections.emptyList();
  }

  /**
   * Handles an invalid field scenario with the parent object context.
   *
   * @param instance the parent object (used for context)
   * @param attemptedValue the value that failed validation
   * @return a collection of validation errors
   */
  default Collection<Error> handle(final Object instance, final P attemptedValue) {
    return handle(attemptedValue);
  }

}
