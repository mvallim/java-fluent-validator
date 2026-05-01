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
 * A builder wrapper for {@link Function} that provides null-safe function composition
 * and execution. This class wraps a function and ensures that null inputs are handled
 * gracefully by returning null instead of throwing exceptions.
 *
 * <p>The FunctionBuilder provides a fluent API for creating and composing functions
 * while maintaining null safety throughout the chain of operations.</p>
 *
 * <p>Example usage:</p>
 * <pre>{@code
 * Function<String, Integer> stringToLength = FunctionBuilder.of(String::length);
 * Integer result = stringToLength.apply(null); // Returns null instead of NPE
 *
 * Function<String, String> composed = FunctionBuilder.of(String::toLowerCase)
 *     .andThen(s -> s + "!");
 * }</pre>
 *
 * @param <I> the type of the input to the function
 * @param <O> the type of the result of the function
 */
public final class FunctionBuilder<I, O> implements Function<I, O> {

  /**
   * The wrapped function that will be executed when apply is called.
   */
  private final Function<I, O> function;

  /**
   * Private constructor to create a new FunctionBuilder instance.
   * Use {@link #of(Function)} to create instances.
   *
   * @param function the function to wrap, must not be null
   * @throws NullPointerException if function is null
   */
  private FunctionBuilder(final Function<I, O> function) {
    this.function = function;
  }

  /**
   * Creates a new FunctionBuilder that wraps the given function, providing
   * null-safe execution.
   * <p>
   * This is the factory method for creating FunctionBuilder instances. The
   * returned function will return null when applied with a null input, instead
   * of throwing a NullPointerException.
   * </p>
   *
   * @param <I> the type of the input to the function
   * @param <O> the type of the result of the function
   * @param function the function to wrap, must not be null
   * @return a new FunctionBuilder instance wrapping the given function
   * @throws NullPointerException if function is null
   */
  public static <I, O> Function<I, O> of(final Function<I, O> function) {
    return new FunctionBuilder<>(function);
  }

  /**
   * Applies this function to the given argument with null-safe behavior.
   *
   * <p>If the input value is null, this method returns null without
   * calling the wrapped function. Otherwise, it applies the wrapped
   * function to the input value.</p>
   *
   * @param value the function argument
   * @return the function result, or null if the input value is null
   */
  @Override
  public O apply(final I value) {
    return Objects.nonNull(value) ? function.apply(value) : null;
  }

  /**
   * Returns a composed function that first applies this function to its input,
   * and then applies the {@code after} function to the result, maintaining
   * null-safe behavior throughout the composition.
   *
   * <p>If either this function or the after function would receive a null input,
   * the entire composition returns null without executing subsequent functions.</p>
   *
   * @param <V> the type of output of the after function, and of the composed function
   * @param after the function to apply after this function is applied
   * @return a composed function that first applies this function and then applies the
   *         {@code after} function
   * @throws NullPointerException if after is null
   *
   * @see Function#andThen(Function)
   */
  @Override
  public <V> Function<I, V> andThen(final Function<? super O, ? extends V> after) {
    return of(i -> of(after).apply(this.apply(i)));
  }

  /**
   * Returns a composed function that first applies the {@code before} function to
   * its input, and then applies this function to the result, maintaining null-safe
   * behavior throughout the composition.
   *
   * <p>If either the before function or this function would receive a null input,
   * the entire composition returns null without executing subsequent functions.</p>
   *
   * @param <V> the type of input to the before function, and to the composed function
   * @param before the function to apply before this function is applied
   * @return a composed function that first applies the {@code before} function and
   *         then applies this function
   * @throws NullPointerException if before is null
   *
   * @see Function#compose(Function)
   */
  @Override
  public <V> Function<V, O> compose(final Function<? super V, ? extends I> before) {
    return of(v -> this.apply(of(before).apply(v)));
  }

}
