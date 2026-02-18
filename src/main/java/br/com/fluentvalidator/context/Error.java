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

/**
 * Represents a validation error context, encapsulating details about a failed validation,
 * including the field name, error message, error code, and the attempted value.
 * <p>
 * Instances of {@code Error} are immutable and can be created using the static {@link #create(String, String, String, Object)} method.
 * </p>
 *
 * <ul>
 *   <li>{@code field}: The name of the field that failed validation.</li>
 *   <li>{@code message}: The error message describing the validation failure.</li>
 *   <li>{@code code}: The error code associated with the validation error.</li>
 *   <li>{@code attemptedValue}: The value that was attempted and caused the error.</li>
 * </ul>
 *
 * Example usage:
 * <pre>
 *   Error error = Error.create("username", "Username must not be empty", "USR_001", null);
 * </pre>
 */
public class Error {

  private final String message;

  private final String field;

  private final Object attemptedValue;

  private final String code;

  /**
   * Creates a new Error instance with the specified field, message, code, and attempted value.
   *
   * @param field the name of the field that failed validation
   * @param message the validation error message
   * @param code the error code associated with this validation error
   * @param attemptedValue the value that was attempted and failed validation
   * @return a new Error instance with the provided parameters
   */
  public static Error create(final String field, final String message, final String code, final Object attemptedValue) {
    return new Error(field, message, code, attemptedValue);
  }

  /**
   * Constructs an {@code Error} instance with the specified field, message, code, and attempted value.
   *
   * @param field the name of the field where the error occurred
   * @param message the error message describing the validation failure
   * @param code the error code associated with the validation error
   * @param attemptedValue the value that was attempted and caused the error
   */
  protected Error(final String field, final String message, final String code, final Object attemptedValue) {
    this.field = field;
    this.message = message;
    this.code = code;
    this.attemptedValue = attemptedValue;
  }

  /**
   * Returns the name of the field associated with this error.
   *
   * @return the field name as a {@code String}
   */
  public String getField() {
    return field;
  }

  /**
   * Returns the error message associated with this error context.
   *
   * @return the error message as a {@code String}
   */
  public String getMessage() {
    return message;
  }

  /**
   * Returns the error code associated with this error.
   *
   * @return the error code as a {@code String}
   */
  public String getCode() {
    return code;
  }

  /**
   * Returns the value that was attempted during validation.
   *
   * @return the attempted value, which may be null if not set
   */
  public Object getAttemptedValue() {
    return attemptedValue;
  }

  /**
   * Returns a string representation of the Error object, including the message,
   * field, attempted value, and code properties.
   *
   * @return a formatted string describing the Error instance
   */
  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder();

    builder.append("Error [");
    builder.append("message=");
    builder.append(message);
    builder.append(", ");
    builder.append("field=");
    builder.append(field);
    builder.append(", ");
    builder.append("attemptedValue=");
    builder.append(attemptedValue);
    builder.append(", ");
    builder.append("code=");
    builder.append(code);
    builder.append("]");

    return builder.toString();
  }

}
