package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface HandleInvalidField<T, P, W extends When<T, P, W>> {

  /**
   *
   * @param when
   * @return
   */
  When<T, P, W> when(final Predicate<P> when);

  /**
   *
   * @param predicate
   * @return
   */
  Must<T, P, W> must(final Predicate<P> predicate);

}
