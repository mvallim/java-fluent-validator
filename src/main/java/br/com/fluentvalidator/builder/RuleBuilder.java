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
 * Base interface for building validation rules in a fluent validation API.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 * @param <W> the type of the When condition builder
 * @param <N> the type of the Whenever condition builder
 */
interface RuleBuilder<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> {

  /**
   * Specifies a predicate that, when true, triggers the validation rules.
   *
   * @param predicate the predicate to evaluate against the property value
   * @return the Whenever builder for chaining additional validation configuration
   */
  N whenever(final Predicate<P> predicate);

  /**
   * Specifies the predicate that must be satisfied for the validation to pass.
   *
   * @param predicate the predicate to validate against the property value
   * @return the Must builder for chaining conditional validation rules
   */
  Must<T, P, W, N> must(final Predicate<P> predicate);

}
