package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

interface RuleBuilder<T, P, W extends When<T, P, W>> {

  /**
   *
   * @param predicate
   * @return
   */
  W whenever(final Predicate<P> predicate);

  /**
   *
   * @param predicate
   * @return
   */
  Must<T, P, W> must(final Predicate<P> predicate);

}
