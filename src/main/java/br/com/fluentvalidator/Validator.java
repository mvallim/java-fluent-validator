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

package br.com.fluentvalidator;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.rule.Rule;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public interface Validator<T> extends Rule<T> {

  /**
   *
   */
  void rules();

  /**
   *
   */
  void failFastRule();

  /**
   *
   * @return Current count element on collection
   */
  Integer getCounter();

  /**
   *
   * @param property
   */
  void setPropertyOnContext(final String property);

  /**
   *
   * @param property
   * @param clazz
   * @return
   */
  <P> P getPropertyOnContext(final String property, final Class<P> clazz);

  /**
   *
   * @param instance
   * @return
   */
  ValidationResult validate(final T instance);

  /**
   *
   * @param instance
   * @param transform
   * @return
   */
  <E> E validate(final T instance, final ValidationResultTransform<E> transform);

  /**
   *
   * @param instances
   * @return
   */
  List<ValidationResult> validate(final Collection<T> instances);

  /**
   *
   * @param instances
   * @param transform
   * @return
   */
  <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> transform);

  /**
   *
   * @param <P>
   * @param function
   * @return
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function);

  /**
   *
   * @param <P>
   * @param fieldName
   * @param function
   * @return
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final String fieldName, final Function<T, P> function);

  /**
   *
   * @param <P>
   * @param function
   * @return
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function);

  /**
   *
   * @param <P>
   * @param fieldName
   * @param function
   * @return
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final String fieldName, final Function<T, Collection<P>> function);
}
