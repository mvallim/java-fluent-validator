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

import java.util.function.Predicate;

/**
 * Builder interface for defining the predicate that must be satisfied for validation to pass.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 * @param <W> the type of the When condition builder
 * @param <N> the type of the Whenever condition builder
 */
public interface Must<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> extends When<T, P, W, N> {

  /**
   * Adds an additional condition that must also be satisfied for the validation to be applied.
   *
   * @param when the predicate that must be true for the validation to proceed
   * @return the When builder for chaining additional configuration
   */
  When<T, P, W, N> when(final Predicate<P> when);

}
