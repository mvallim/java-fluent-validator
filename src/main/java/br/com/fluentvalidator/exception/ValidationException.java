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
 * Abstract base class for validation exceptions in the FluentValidator framework.
 * <p>
 * This class provides a foundation for creating custom validation exceptions that
 * carry validation results. It extends {@link RuntimeException} and includes
 * factory methods for creating instances of validation exception subclasses.
 * </p>
 */
public abstract class ValidationException extends RuntimeException {

  private static final long serialVersionUID = 2274879814700248645L;

  private final transient ValidationResult validationResult;

  /**
   * Constructs a new ValidationException with the specified validation result.
   * <p>
   * The exception message is automatically generated from the validation result's
   * string representation.
   * </p>
   *
   * @param validationResult the validation result containing error details
   * @throws NullPointerException if validationResult is null
   */
  protected ValidationException(final ValidationResult validationResult) {
    super(validationResult.toString());
    this.validationResult = validationResult;
  }

  /**
   * Returns the validation result associated with this exception.
   * <p>
   * The validation result contains detailed information about validation
   * errors that occurred during the validation process.
   * </p>
   *
   * @return the validation result, never null
   */
  public ValidationResult getValidationResult() {
    return validationResult;
  }

  /**
   * Creates a new instance of the specified validation exception class using
   * the current validation context's result.
   * <p>
   * This is a convenience method that retrieves the validation result from
   * the current {@link ValidationContext} and creates an exception instance.
   * </p>
   *
   * @param <T> the type of validation exception to create
   * @param exceptionClass the class of the validation exception to instantiate
   * @return a new RuntimeException instance of the specified type
   * @throws RuntimeException if the exception class doesn't have the required constructor
   *                         or if instantiation fails
   * @see #create(Class, ValidationResult)
   */
  public static <T extends ValidationException> RuntimeException create(final Class<T> exceptionClass) {
    return create(exceptionClass, ValidationContext.get().getValidationResult());
  }

  /**
   * Creates a new instance of the specified validation exception class with
   * the provided validation result.
   * <p>
   * This method uses reflection to instantiate the exception class. The target
   * class must have a constructor that accepts a single {@link ValidationResult}
   * parameter.
   * </p>
   *
   * @param <T> the type of validation exception to create
   * @param exceptionClass the class of the validation exception to instantiate
   * @param validationResult the validation result to associate with the exception
   * @return a new RuntimeException instance of the specified type
   * @throws RuntimeException if the exception class doesn't have a constructor
   *                         accepting ValidationResult, or if instantiation fails
   *                         due to security restrictions, illegal access, or
   *                         invocation target exceptions
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
