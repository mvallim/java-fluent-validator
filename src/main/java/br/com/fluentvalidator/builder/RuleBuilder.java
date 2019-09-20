package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

interface RuleBuilder<T, P, W extends When<T, P, W>> {

  W whenever(final Predicate<P> predicate);

  Must<T, P, W> must(final Predicate<P> predicate);

}
