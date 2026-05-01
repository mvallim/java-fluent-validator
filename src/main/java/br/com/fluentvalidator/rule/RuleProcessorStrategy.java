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
 * Implementations define how rules are applied, such as fail-fast (stop on first failure)
 * or default (process all rules).
 */
public interface RuleProcessorStrategy {

  /**
   * Processes a single rule against a value, using the provided object as context.
   * The rule is only applied if it supports the value.
   *
   * @param <E> the type of the value
   * @param obj the context object (typically the parent object being validated)
   * @param value the value to validate
   * @param rule the rule to apply
   * @return {@code true} if the rule passes or is not supported, {@code false} otherwise
   */
  default <E> boolean process(final Object obj, final E value, final Rule<E> rule) {
    return Boolean.FALSE.equals(rule.support(value)) || rule.apply(obj, value);
  }

  /**
   * Processes a single rule against a value without a parent context object.
   * The rule is only applied if it supports the value.
   *
   * @param <E> the type of the value
   * @param value the value to validate
   * @param rule the rule to apply
   * @return {@code true} if the rule passes or is not supported, {@code false} otherwise
   */
  default <E> boolean process(final E value, final Rule<E> rule) {
    return Boolean.FALSE.equals(rule.support(value)) || rule.apply(value);
  }

  /**
   * Processes a single rule against each element in a collection of values, using the provided object as context.
   * Uses a {@link ProcessorContext} to track processing state across elements.
   *
   * @param <E> the type of elements in the collection
   * @param obj the context object (typically the parent object being validated)
   * @param values the collection of values to validate
   * @param rule the rule to apply to each element
   * @return {@code true} if all elements pass the rule, {@code false} otherwise
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
   * Processes a single rule against each element in a collection of values without a parent context object.
   * Uses a {@link ProcessorContext} to track processing state across elements.
   *
   * @param <E> the type of elements in the collection
   * @param values the collection of values to validate
   * @param rule the rule to apply to each element
   * @return {@code true} if all elements pass the rule, {@code false} otherwise
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
   * Processes a collection of rules against a value, using the provided object as context.
   *
   * @param <E> the type of the value
   * @param obj the context object (typically the parent object being validated)
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @return {@code true} if all rules pass, {@code false} otherwise
   */
  default <E> boolean process(final Object obj, final E value, final Collection<Rule<E>> rules) {
    return rules.stream().map(rule -> this.process(obj, value, rule)).collect(Collectors.toList()).stream()
      .allMatch(result -> result);
  }

  /**
   * Processes a collection of rules against a value without a parent context object.
   *
   * @param <E> the type of the value
   * @param value the value to validate
   * @param rules the collection of rules to apply
   * @return {@code true} if all rules pass, {@code false} otherwise
   */
  default <E> boolean process(final E value, final Collection<Rule<E>> rules) {
    return rules.stream().map(rule -> this.process(value, rule)).collect(Collectors.toList()).stream()
      .allMatch(result -> result);
  }

  /**
   * Returns a fail-fast rule processor strategy. Validation stops on the first failure.
   *
   * @return a fail-fast rule processor strategy
   */
  public static RuleProcessorStrategy getFailFast() {
    return new RuleProcessorFailFast();
  }

  /**
   * Returns a default rule processor strategy. All rules are processed even if some fail.
   *
   * @return a default rule processor strategy
   */
  public static RuleProcessorStrategy getDefault() {
    return new RuleProcessorDefault();
  }

}
