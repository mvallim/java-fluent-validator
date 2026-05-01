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

import java.util.Collection;

/**
 * Fail-fast implementation of {@link RuleProcessorStrategy} that stops processing validation rules
 * as soon as the first failure is encountered. This strategy is useful when you want to avoid
 * unnecessary processing after a validation error has been detected.
 */
class RuleProcessorFailFast implements RuleProcessorStrategy {

  /**
   * Processes a collection of rules against a value using fail-fast semantics.
   * Stops processing on the first rule that fails.
   *
   * @param <E> the type of the value
   * @param obj the context object (typically the parent object being validated)
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @return {@code true} if all rules pass, {@code false} otherwise
   */
  @Override
  public <E> boolean process(final Object obj, final E value, final Collection<Rule<E>> rules) {
    return rules.stream().allMatch(rule -> this.process(obj, value, rule));
  }

  /**
   * Processes a collection of rules against a value without a parent context object, using fail-fast semantics.
   * Stops processing on the first rule that fails.
   *
   * @param <E> the type of the value
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @return {@code true} if all rules pass, {@code false} otherwise
   */
  @Override
  public <E> boolean process(final E value, final Collection<Rule<E>> rules) {
    return rules.stream().allMatch(rule -> this.process(value, rule));
  }

}
