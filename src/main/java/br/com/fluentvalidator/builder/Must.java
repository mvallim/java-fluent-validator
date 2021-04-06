package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface Must<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> extends When<T, P, W, N> {

  /**
   *
   * @param when
   * @return
   */
  When<T, P, W, N> when(final Predicate<P> when);

}
