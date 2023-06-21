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

public interface RuleProcessorStrategy {

  default <E> boolean process(final Object obj, final E value, final Rule<E> rule) {
    return Boolean.FALSE.equals(rule.support(value)) || rule.apply(obj, value);
  }

  default <E> boolean process(final E value, final Rule<E> rule) {
    return Boolean.FALSE.equals(rule.support(value)) || rule.apply(value);
  }

  default <E> boolean process(final Object obj, final Collection<E> values, final Rule<E> rule) {
    ProcessorContext.get().create();
    final boolean allMatch = values.stream().map(value -> {
      ProcessorContext.get().inc();
      return this.process(obj, value, rule);
    }).collect(Collectors.toList()).stream().allMatch(result -> result);
    ProcessorContext.get().remove();
    return allMatch;
  }

  default <E> boolean process(final Collection<E> values, final Rule<E> rule) {
    ProcessorContext.get().create();
    final boolean allMatch = values.stream().map(value -> {
      ProcessorContext.get().inc();
      return this.process(value, rule);
    }).collect(Collectors.toList()).stream().allMatch(result -> result);
    ProcessorContext.get().remove();
    return allMatch;
  }

  default <E> boolean process(final Object obj, final E value, final Collection<Rule<E>> rules) {
    return rules.stream().map(rule -> this.process(obj, value, rule)).collect(Collectors.toList()).stream()
      .allMatch(result -> result);
  }

  default <E> boolean process(final E value, final Collection<Rule<E>> rules) {
    return rules.stream().map(rule -> this.process(value, rule)).collect(Collectors.toList()).stream()
      .allMatch(result -> result);
  }

  public static RuleProcessorStrategy getFailFast() {
    return new RuleProcessorFailFast();
  }

  public static RuleProcessorStrategy getDefault() {
    return new RuleProcessorDefault();
  }

}
