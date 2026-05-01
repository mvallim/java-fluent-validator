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

package br.com.fluentvalidator.exception;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationResult;

/**
 * Base class for validation exceptions.
 * Thrown when a critical validation rule fails.
 */
public abstract class ValidationException extends RuntimeException {

  private static final long serialVersionUID = 2274879814700248645L;

  private final transient ValidationResult validationResult;

  /**
   * Constructs a new ValidationException with the specified validation result.
   *
   * @param validationResult the validation result containing the errors
   */
  protected ValidationException(final ValidationResult validationResult) {
    super(validationResult.toString());
    this.validationResult = validationResult;
  }

  /**
   * Returns the validation result associated with this exception.
   *
   * @return the validation result
   */
  public ValidationResult getValidationResult() {
    return validationResult;
  }

  /**
   * Creates a new ValidationException instance of the specified class.
   *
   * @param <T> the type of ValidationException to create
   * @param exceptionClass the class of the exception to create
   * @return a new ValidationException instance
   */
  public static <T extends ValidationException> RuntimeException create(final Class<T> exceptionClass) {
    return create(exceptionClass, ValidationContext.get().getValidationResult());
  }

  /**
   * Creates a new ValidationException instance of the specified class with the given validation result.
   *
   * @param <T> the type of ValidationException to create
   * @param exceptionClass the class of the exception to create
   * @param validationResult the validation result to associate with the exception
   * @return a new ValidationException instance
   */
  public static <T extends ValidationException> RuntimeException create(final Class<T> exceptionClass, final ValidationResult validationResult) {
    try {
      final Constructor<? extends ValidationException> ctor = exceptionClass.getConstructor(ValidationResult.class);
      return ctor.newInstance(validationResult);
    } catch (final NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
      return new RuntimeException("Constructor in class not found (ValidationResult validationResult)", e);
    }
  }

}
