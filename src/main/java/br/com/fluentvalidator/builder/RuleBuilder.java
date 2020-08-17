package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

interface RuleBuilder<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> {

  /**
   *
   * @param predicate
   * @return
   */
  N whenever(final Predicate<P> predicate);

  /**
   *
   * @param predicate
   * @return
   */
  Must<T, P, W, N> must(final Predicate<P> predicate);

}
