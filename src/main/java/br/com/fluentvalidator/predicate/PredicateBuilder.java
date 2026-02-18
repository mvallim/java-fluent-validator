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

package br.com.fluentvalidator.predicate;

import java.util.function.Predicate;

/**
 * A builder class that wraps a {@link Predicate} and provides a fluent interface
 * for predicate operations. This class implements the {@link Predicate} interface
 * and delegates the actual test logic to the wrapped predicate.
 *
 * <p>This class is designed to be used as part of a fluent validation framework,
 * allowing for more readable and maintainable predicate compositions.</p>
 *
 * <p>Example usage:</p>
 * <pre>{@code
 * Predicate<String> notNull = PredicateBuilder.from(Objects::nonNull);
 * boolean result = notNull.test("example");
 * }</pre>
 *
 * @param <T> the type of the input to the predicate
 */
public final class PredicateBuilder<T> implements Predicate<T> {

  /**
   * The wrapped predicate that performs the actual test logic.
   */
  private final Predicate<T> predicate;

  /**
   * Creates a new PredicateBuilder instance wrapping the given predicate.
   * This is the preferred way to create instances of this class.
   *
   * @param <T> the type of the input to the predicate
   * @param predicate the predicate to wrap; must not be null
   * @return a new PredicateBuilder instance wrapping the given predicate
   * @throws NullPointerException if the predicate is null
   */
  public static <T> Predicate<T> from(final Predicate<T> predicate) {
    return new PredicateBuilder<>(predicate);
  }

  /**
   * Private constructor to create a new PredicateBuilder instance.
   * Use the {@link #from(Predicate)} factory method instead.
   *
   * @param predicate the predicate to wrap; must not be null
   * @throws NullPointerException if the predicate is null
   */
  private PredicateBuilder(final Predicate<T> predicate) {
    this.predicate = predicate;
  }

  /**
   * Evaluates this predicate on the given argument by delegating to the
   * wrapped predicate.
   *
   * @param value the input argument to test
   * @return {@code true} if the input argument matches the predicate,
   *         otherwise {@code false}
   */
  @Override
  public boolean test(final T value) {
    return predicate.test(value);
  }

}
