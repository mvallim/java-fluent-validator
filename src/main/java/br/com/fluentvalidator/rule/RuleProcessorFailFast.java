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
 * Fail-fast implementation of RuleProcessorStrategy.
 * This strategy stops validation on the first failure.
 */
class RuleProcessorFailFast implements RuleProcessorStrategy {

  /**
   * Processes rules against a value with an associated object.
   * Stops processing on the first failure.
   *
   * @param obj the parent object (used for context)
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @param <E> the type of the value
   * @return true if all rules pass, false if any fails
   */
  @Override
  public <E> boolean process(final Object obj, final E value, final Collection<Rule<E>> rules) {
    return rules.stream().allMatch(rule -> this.process(obj, value, rule));
  }

  /**
   * Processes rules against a value.
   * Stops processing on the first failure.
   *
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @param <E> the type of the value
   * @return true if all rules pass, false if any fails
   */
  @Override
  public <E> boolean process(final E value, final Collection<Rule<E>> rules) {
    return rules.stream().allMatch(rule -> this.process(value, rule));
  }

}
