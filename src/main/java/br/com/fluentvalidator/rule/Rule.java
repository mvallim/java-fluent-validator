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

package br.com.fluentvalidator.rule;

/**
 * Represents a validation rule that can be applied to an object of type {@code T}.
 * Rules can either be applied directly to an object, or to a specific value extracted from the object.
 *
 * @param <T> the type of object the rule can be applied to
 */
public interface Rule<T> {

  /**
   * Applies this rule to the given instance.
   *
   * @param instance the object to apply the rule to
   * @return {@code true} if the rule is satisfied, {@code false} otherwise
   */
  default boolean apply(final T instance) {
    return true;
  }

  /**
   * Applies this rule to the given value, using the provided instance as context (e.g., for error message generation).
   *
   * @param instance the context object (typically the parent object being validated)
   * @param value the value to apply the rule to
   * @return {@code true} if the rule is satisfied, {@code false} otherwise
   */
  default boolean apply(final Object instance, final T value) {
    return apply(value);
  }

  /**
   * Checks if this rule supports the given instance. Rules may only be applicable to certain objects.
   *
   * @param instance the object to check support for
   * @return {@code true} if this rule supports the instance, {@code false} otherwise
   */
  default boolean support(final T instance) {
    return true;
  }

}
