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
