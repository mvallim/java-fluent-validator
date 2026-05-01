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

package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.exception.ValidationException;

/**
 * Builder interface that allows associating a validator and configuring critical validation behavior.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 * @param <W> the type of the When condition builder
 * @param <N> the type of the Whenever condition builder
 */
public interface WithValidator<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> extends RuleBuilder<T, P, W, N> {

  /**
   * Marks the validation rule as critical, causing validation to stop on failure.
   *
   * @return the Critical builder for chaining additional configuration
   */
  Critical<T, P, W, N> critical();

  /**
   * Marks the validation rule as critical with a custom exception class, causing validation to stop on failure.
   *
   * @param clazz the custom ValidationException class to be thrown on critical failure
   * @return the Critical builder for chaining additional configuration
   */
  Critical<T, P, W, N> critical(final Class<? extends ValidationException> clazz);

}
