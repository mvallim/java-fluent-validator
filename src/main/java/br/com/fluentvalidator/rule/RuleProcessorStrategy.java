package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.stream.Collectors;

public interface RuleProcessorStrategy {

    default <E> boolean process(final E value, final Rule<E> rule) {
        return Boolean.FALSE.equals(rule.support(value)) || rule.apply(value);
    }

    default <E> boolean process(final Collection<E> values, final Rule<E> rule) {
        return values.stream().map(value -> this.process(value, rule)).collect(Collectors.toList()).stream().allMatch(result -> result);
    }

    default <E> boolean process(final E value, final Collection<Rule<E>> rules) {
        return rules.stream().map(rule -> this.process(value, rule)).collect(Collectors.toList()).stream().allMatch(result -> result);
    }

    public static RuleProcessorStrategy getFailFast() {
        return new RuleProcessorFailFast();
    }

    public static RuleProcessorStrategy getDefault() {
        return new RuleProcessorDefault();
    }

}