package br.com.fluentvalidator.function;

import java.util.Objects;
import java.util.function.Function;

public final class FunctionBuilder<I, O> implements Function<I, O> {

  private final Function<I, O> function;

  /**
   *
   * @param function
   */
  private FunctionBuilder(final Function<I, O> function) {
    this.function = function;
  }

  public static <I, O> Function<I, O> of(final Function<I, O> function) {
    return new FunctionBuilder<>(function);
  }

  @Override
  public O apply(final I value) {
    return Objects.nonNull(value) ? function.apply(value) : null;
  }

  @Override
  public <V> Function<I, V> andThen(final Function<? super O, ? extends V> after) {
    return of(i -> of(after).apply(this.apply(i)));
  }

  @Override
  public <V> Function<V, O> compose(final Function<? super V, ? extends I> before) {
    return of(v -> this.apply(of(before).apply(v)));
  }

}
