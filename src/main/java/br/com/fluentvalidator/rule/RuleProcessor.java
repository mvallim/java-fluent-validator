package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.stream.Collectors;

public final class RuleProcessor {

    private RuleProcessor() {
        super();
    }

    public static <E> boolean process(final E value, final Rule<E> rule) {
        return Boolean.FALSE.equals(rule.support(value)) || rule.apply(value);
    }

    public static <E> boolean process(final Collection<E> values, final Rule<E> rule) {
        return values.stream().map(value -> RuleProcessor.process(value, rule)).collect(Collectors.toList()).stream().allMatch(result -> result);
    }

    public static <E> boolean process(final E value, final Collection<Rule<E>> rules) {
        return rules.stream().map(rule -> RuleProcessor.process(value, rule)).collect(Collectors.toList()).stream().allMatch(result -> result);
    }

}
