/*
 * Copyright 2023 the original author or authors.
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

package br.com.fluentvalidator.exception;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationResult;

public abstract class ValidationException extends RuntimeException {

  private static final long serialVersionUID = 2274879814700248645L;

  private final transient ValidationResult validationResult;

  protected ValidationException(final ValidationResult validationResult) {
    super(validationResult.toString());
    this.validationResult = validationResult;
  }

  /**
   *
   * @return
   */
  public ValidationResult getValidationResult() {
    return validationResult;
  }

  /**
   *
   * @param exceptionClass
   * @return
   */
  public static <T extends ValidationException> RuntimeException create(final Class<T> exceptionClass) {
    return create(exceptionClass, ValidationContext.get().getValidationResult());
  }

  /**
   *
   * @param exceptionClass
   * @param validationResult
   * @return
   */
  public static <T extends ValidationException> RuntimeException create(final Class<T> exceptionClass, final ValidationResult validationResult) {
    try {
      final Constructor<? extends ValidationException> ctor =
          exceptionClass.getConstructor(ValidationResult.class);
      return ctor.newInstance(validationResult);
    } catch (final NoSuchMethodException | SecurityException | InstantiationException
        | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
      return new RuntimeException(
          "Constructor in class not found (ValidationResult validationResult)", e);
    }
  }

}
