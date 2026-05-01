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
import java.util.stream.Collectors;

import br.com.fluentvalidator.context.ProcessorContext;
import br.com.fluentvalidator.context.ProcessorContext.Context;

/**
 * Strategy interface for processing validation rules.
 * Different implementations can provide different processing behaviors (e.g., fail-fast vs. collect all errors).
 */
public interface RuleProcessorStrategy {

  /**
   * Processes a single rule against a value, with an associated object for context.
   *
   * @param obj the parent object (used for context)
   * @param value the value to validate
   * @param rule the rule to apply
   * @param <E> the type of the value
   * @return true if the rule passes or is not supported, false otherwise
   */
  default <E> boolean process(final Object obj, final E value, final Rule<E> rule) {
    return Boolean.FALSE.equals(rule.support(value)) || rule.apply(obj, value);
  }

  /**
   * Processes a single rule against a value.
   *
   * @param value the value to validate
   * @param rule the rule to apply
   * @param <E> the type of the value
   * @return true if the rule passes or is not supported, false otherwise
   */
  default <E> boolean process(final E value, final Rule<E> rule) {
    return Boolean.FALSE.equals(rule.support(value)) || rule.apply(value);
  }

  /**
   * Processes a rule against a collection of values, with an associated object for context.
   * Uses processor context to track the number of validations.
   *
   * @param obj the parent object (used for context)
   * @param values the collection of values to validate
   * @param rule the rule to apply to each value
   * @param <E> the type of elements in the collection
   * @return true if all validations pass, false otherwise
   */
  default <E> boolean process(final Object obj, final Collection<E> values, final Rule<E> rule) {
    try (final Context context = ProcessorContext.get()) {
      return values.stream().map(value -> {
        context.inc();
        return this.process(obj, value, rule);
      }).collect(Collectors.toList()).stream().allMatch(result -> result);
    }
  }

  /**
   * Processes a rule against a collection of values.
   * Uses processor context to track the number of validations.
   *
   * @param values the collection of values to validate
   * @param rule the rule to apply to each value
   * @param <E> the type of elements in the collection
   * @return true if all validations pass, false otherwise
   */
  default <E> boolean process(final Collection<E> values, final Rule<E> rule) {
    try (final Context context = ProcessorContext.get()) {
      return values.stream().map(value -> {
        context.inc();
        return this.process(value, rule);
      }).collect(Collectors.toList()).stream().allMatch(result -> result);
    }
  }

  /**
   * Processes a collection of rules against a value, with an associated object for context.
   *
   * @param obj the parent object (used for context)
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @param <E> the type of the value
   * @return true if all rules pass, false otherwise
   */
  default <E> boolean process(final Object obj, final E value, final Collection<Rule<E>> rules) {
    return rules.stream().map(rule -> this.process(obj, value, rule)).collect(Collectors.toList()).stream().allMatch(result -> result);
  }

  /**
   * Processes a collection of rules against a value.
   *
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @param <E> the type of the value
   * @return true if all rules pass, false otherwise
   */
  default <E> boolean process(final E value, final Collection<Rule<E>> rules) {
    return rules.stream().map(rule -> this.process(value, rule)).collect(Collectors.toList()).stream().allMatch(result -> result);
  }

  /**
   * Returns a fail-fast rule processor strategy.
   * In fail-fast mode, validation stops on the first failure.
   *
   * @return a fail-fast rule processor strategy
   */
  public static RuleProcessorStrategy getFailFast() {
    return new RuleProcessorFailFast();
  }

  /**
   * Returns the default rule processor strategy.
   * The default strategy collects all validation errors.
   *
   * @return the default rule processor strategy
   */
  public static RuleProcessorStrategy getDefault() {
    return new RuleProcessorDefault();
  }

}
