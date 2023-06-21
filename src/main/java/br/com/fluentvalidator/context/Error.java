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

package br.com.fluentvalidator.context;

public class Error {

  private final String message;

  private final String field;

  private final Object attemptedValue;

  private final String code;

  /**
   *
   * @param field
   * @param message
   * @param code
   * @param attemptedValue
   * @return
   */
  public static Error create(final String field, final String message, final String code, final Object attemptedValue) {
    return new Error(field, message, code, attemptedValue);
  }

  protected Error(final String field, final String message, final String code, final Object attemptedValue) {
    this.field = field;
    this.message = message;
    this.code = code;
    this.attemptedValue = attemptedValue;
  }

  /**
   *
   * @return
   */
  public String getField() {
    return field;
  }

  /**
   *
   * @return
   */
  public String getMessage() {
    return message;
  }

  /**
   *
   * @return
   */
  public String getCode() {
    return code;
  }

  /**
   *
   * @return
   */
  public Object getAttemptedValue() {
    return attemptedValue;
  }

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
