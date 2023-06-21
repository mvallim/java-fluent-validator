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

public final class LogicalPredicate {

  /**
   *
   * @param <T>
   * @param predicate
   * @return
   */
  static <T> Predicate<T> is(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.and(is -> true));
  }

  /**
   * 
   * @return
   */
  public static Predicate<Boolean> isFalse() {
    return PredicateBuilder.<Boolean>from(not(nullValue())).and(not(isFalse -> isFalse));
  }

  /**
   * 
   * @param <T>
   * @param function
   * @return
   */
  public static <T> Predicate<T> isFalse(final Function<T, Boolean> function) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(function)))
        .and(not(function::apply));
  }

  /**
   * 
   * @return
   */
  public static Predicate<Boolean> isTrue() {
    return PredicateBuilder.<Boolean>from(not(nullValue())).and(is(isTrue -> isTrue));
  }

  /**
   * 
   * @param <T>
   * @param function
   * @return
   */
  public static <T> Predicate<T> isTrue(final Function<T, Boolean> function) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(not(nullValue(function)))
        .and(function::apply);
  }
  
  /**
   *
   * @param <T>
   * @param predicate
   * @return
   */
  public static <T> Predicate<T> not(final Predicate<T> predicate) {
    return PredicateBuilder.<T>from(predicate.negate());
  }

  private LogicalPredicate() {
    super();
  }

}
