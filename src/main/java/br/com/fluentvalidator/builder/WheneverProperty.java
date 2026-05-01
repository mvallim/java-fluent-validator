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

package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.Validator;

/**
 * Builder interface for defining whenever conditions on property validation rules.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 */
public interface WheneverProperty<T, P> extends Whenever<T, P, WhenProperty<T, P>, WheneverProperty<T, P>> {

  /**
   * Associates a validator to be applied to the property value.
   *
   * @param validator the validator to apply to the property
   * @return the WithValidator builder for chaining additional configuration
   */
  WithValidator<T, P, WhenProperty<T, P>, WheneverProperty<T, P>> withValidator(final Validator<P> validator);

}
