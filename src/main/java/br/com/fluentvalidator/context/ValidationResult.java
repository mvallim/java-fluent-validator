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

package br.com.fluentvalidator.context;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;

import br.com.fluentvalidator.exception.ValidationException;

/**
 * Represents the result of a validation operation, containing the validation status
 * and any associated error messages.
 *
 * <p>This class is immutable and thread-safe. It provides factory methods to create
 * instances representing successful or failed validation results.</p>
 */
public final class ValidationResult {

  private final boolean valid;

  private final Collection<Error> errors;

  /**
   * Creates a successful validation result with no errors.
   *
   * @return a ValidationResult instance representing a successful validation
   */
  public static ValidationResult ok() {
    return new ValidationResult(true, new ArrayList<>());
  }

  /**
   * Creates a failed validation result with the specified error messages.
   *
   * @param messages the collection of error messages; if null, an empty collection is used
   * @return a ValidationResult instance representing a failed validation
   */
  public static ValidationResult fail(final Collection<Error> messages) {
    return new ValidationResult(false, Optional.ofNullable(messages).orElse(new ArrayList<>()));
  }

  /**
   * Private constructor to create a ValidationResult instance.
   *
   * @param valid indicates whether the validation was successful
   * @param messages the collection of error messages
   */
  private ValidationResult(final boolean valid, final Collection<Error> messages) {
    this.valid = valid;
    errors = Collections.unmodifiableCollection(messages);
  }

  /**
   * Throws a validation exception if the validation result is invalid.
   *
   * <p>This method provides a convenient way to convert validation failures
   * into exceptions for error handling purposes.</p>
   *
   * @param <T> the type of ValidationException to throw
   * @param clazz the class of the ValidationException to instantiate and throw
   * @throws ValidationException if the validation result is invalid
   */
  public <T extends ValidationException> void isInvalidThrow(final Class<T> clazz) {
    if (!isValid()) {
      throw ValidationException.create(clazz, this);
    }
  }

  /**
   * Checks whether the validation was successful.
   *
   * @return {@code true} if the validation was successful, {@code false} otherwise
   */
  public boolean isValid() {
    return valid;
  }

  /**
   * Returns the collection of validation errors.
   *
   * <p>The returned collection is unmodifiable. If the validation was successful,
   * this collection will be empty.</p>
   *
   * @return an unmodifiable collection of validation errors
   */
  public Collection<Error> getErrors() {
    return errors;
  }

  /**
   * Returns a string representation of this ValidationResult.
   *
   * <p>The string representation includes the validation status and
   * the collection of errors.</p>
   *
   * @return a string representation of this ValidationResult
   */
  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder();
    builder.append("ValidationResult [valid=");
    builder.append(valid);
    builder.append(", ");
    builder.append("errors=");
    builder.append(errors);
    builder.append("]");
    return builder.toString();
  }

}
