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

package br.com.fluentvalidator.function;

import java.util.Objects;
import java.util.function.Function;

/**
 * A builder class that wraps a {@link java.util.function.Function} and provides a fluent interface
 * for function operations. This class implements the {@link java.util.function.Function} interface
 * and delegates the actual apply logic to the wrapped function.
 *
 * <p>This class is designed to be used as part of a fluent validation framework,
 * allowing for more readable and maintainable function compositions.</p>
 *
 * <p>Example usage:</p>
 * <pre>{@code
 * Function<String, Integer> strLength = FunctionBuilder.from(String::length);
 * Integer result = strLength.apply("example");
 * }</pre>
 *
 * @param <I> the type of the input to the function
 * @param <O> the type of the output of the function
 */
public final class FunctionBuilder<I, O> implements Function<I, O> {

  private final Function<I, O> function;

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
