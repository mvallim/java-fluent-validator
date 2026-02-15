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

import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Utility class providing logical predicate operations for fluent validation.
 * This class contains static methods for creating and combining predicates
 * with logical operations such as negation, boolean evaluation, and identity checks.
 *
 * <p>All methods in this class are static and the class cannot be instantiated.</p>
 */
public final class LogicalPredicate {
  
  /**
   * Creates an identity predicate that wraps the given predicate with an additional
   * always-true condition. This method is primarily used for predicate composition
   * and fluent API construction.
   * 
   * @param <T> the type of the input to the predicate
   * @param predicate the predicate to wrap, must not be null
   * @return a new predicate that combines the input predicate with an identity check
   * @throws NullPointerException if predicate is null
   */
  static <T> Predicate<T> is(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.and(is -> true));
  }

  /**
   * Creates a predicate that tests if a Boolean value is false.
   * The predicate first checks that the input is not null, then verifies
   * that the boolean value is false.
   * 
   * @return a predicate that returns true if the input Boolean is false, false otherwise
   */
  public static Predicate<Boolean> isFalse() {
    return PredicateBuilder.<Boolean>from(not(nullValue())).and(not(isFalse -> isFalse));
  }

  /**
   * Creates a predicate that tests if the result of applying a function to an object is false.
   * The predicate performs null checks on both the input object and the function result
   * before evaluating the boolean value.
   * 
   * @param <T> the type of the input to the predicate
   * @param function the function to apply to extract a Boolean value, must not be null
   * @return a predicate that returns true if the function result is false, false otherwise
   * @throws NullPointerException if function is null
   */
  public static <T> Predicate<T> isFalse(final Function<T, Boolean> function) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(function)))
        .and(not(function::apply));
  }
  
  /**
   * Creates a predicate that tests if a Boolean value is true.
   * The predicate first checks that the input is not null, then verifies
   * that the boolean value is true.
   *
   * @return a predicate that returns true if the input Boolean is true, false otherwise
   */
  public static Predicate<Boolean> isTrue() {
    return PredicateBuilder.<Boolean>from(not(nullValue())).and(is(isTrue -> isTrue));
  }

  /**
   * Creates a predicate that tests if the result of applying a function to an object is true.
   * The predicate performs null checks on both the input object and the function result
   * before evaluating the boolean value.
   *
   * @param <T> the type of the input to the predicate
   * @param function the function to apply to extract a Boolean value, must not be null
   * @return a predicate that returns true if the function result is true, false otherwise
   * @throws NullPointerException if function is null
   */
  public static <T> Predicate<T> isTrue(final Function<T, Boolean> function) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(function)))
        .and(function::apply);
  }

  /**
   * Creates a predicate that represents the logical negation of the given predicate.
   * This is a convenience method that wraps the standard {@link Predicate#negate()} method
   * within the fluent validation framework.
   *
   * @param <T> the type of the input to the predicate
   * @param predicate the predicate to negate, must not be null
   * @return a predicate that represents the logical negation of the input predicate
   * @throws NullPointerException if predicate is null
   */
  public static <T> Predicate<T> not(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.negate());
  }

  /**
   * Private constructor to prevent instantiation of this utility class.
   */
  private LogicalPredicate() {
    super();
  }

}
