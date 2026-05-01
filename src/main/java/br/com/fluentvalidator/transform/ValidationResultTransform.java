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
 * Interface for transforming a ValidationResult into a different type.
 * Allows custom transformation of validation results.
 *
 * @param <E> the type of the transformed result
 */
public interface ValidationResultTransform<E> {

  /**
   * Transforms a ValidationResult into the target type.
   *
   * @param validationResult the validation result to transform
   * @return the transformed result
   */
  E transform(final ValidationResult validationResult);

}
