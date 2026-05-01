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

package br.com.fluentvalidator.transform;

import br.com.fluentvalidator.context.ValidationResult;

/**
 * A functional interface for transforming {@link ValidationResult} into another type.
 * <p>
 * This interface is typically used to convert validation results into custom
 * response objects, such as API error responses or domain-specific result types.
 * </p>
 *
 * @param <E> the type of the transformed result
 */
public interface ValidationResultTransform<E> {

  /**
   * Transforms a {@link ValidationResult} into an object of type {@code E}.
   * <p>
   * Implementations of this method should extract relevant information from
   * the validation result (such as errors, messages, etc.) and convert it
   * into the desired output type.
   * </p>
   *
   * @param validationResult the validation result to transform, must not be null
   * @return the transformed result of type {@code E}
   */
  E transform(final ValidationResult validationResult);

}
