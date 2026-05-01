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
 * Represents the result of a validation operation.
 * Contains information about whether the validation passed and any errors that occurred.
 */
public final class ValidationResult {

  private final boolean valid;

  private final Collection<Error> errors;

  /**
   * Creates a successful validation result with no errors.
   *
   * @return a ValidationResult indicating success
   */
  public static ValidationResult ok() {
    return new ValidationResult(true, new ArrayList<>());
  }

  /**
   * Creates a failed validation result with the specified errors.
   *
   * @param messages the collection of validation errors
   * @return a ValidationResult indicating failure with the given errors
   */
  public static ValidationResult fail(final Collection<Error> messages) {
    return new ValidationResult(false, Optional.ofNullable(messages).orElse(new ArrayList<>()));
  }

  /**
   * Private constructor to create a ValidationResult instance.
   *
   * @param valid whether the validation passed
   * @param messages the collection of validation errors
   */
  private ValidationResult(final boolean valid, final Collection<Error> messages) {
    this.valid = valid;
    errors = Collections.unmodifiableCollection(messages);
  }

  /**
   * Throws a ValidationException if the validation result is invalid.
   *
   * @param <T> the type of ValidationException to throw
   * @param clazz the class of the exception to throw
   * @throws T if the validation result is invalid
   */
  public <T extends ValidationException> void isInvalidThrow(final Class<T> clazz) {
    if (!isValid()) {
      throw ValidationException.create(clazz, this);
    }
  }

  /**
   * Returns whether the validation passed.
   *
   * @return true if the validation passed, false otherwise
   */
  public boolean isValid() {
    return valid;
  }

  /**
   * Returns the collection of validation errors.
   *
   * @return an unmodifiable collection of validation errors
   */
  public Collection<Error> getErrors() {
    return errors;
  }

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
