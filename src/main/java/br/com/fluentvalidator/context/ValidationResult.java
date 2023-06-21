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

public final class ValidationResult {

  private final boolean valid;

  private final Collection<Error> errors;

  /**
   *
   * @return
   */
  public static ValidationResult ok() {
    return new ValidationResult(true, new ArrayList<>());
  }

  /**
   *
   * @param messages
   * @return
   */
  public static ValidationResult fail(final Collection<Error> messages) {
    return new ValidationResult(false, Optional.ofNullable(messages).orElse(new ArrayList<>()));
  }

  private ValidationResult(final boolean valid, final Collection<Error> messages) {
    this.valid = valid;
    errors = Collections.unmodifiableCollection(messages);
  }

  /**
   *
   * @param clazz
   */
  public <T extends ValidationException> void isInvalidThrow(final Class<T> clazz) {
    if (!isValid()) {
      throw ValidationException.create(clazz, this);
    }
  }

  /**
   *
   * @return
   */
  public boolean isValid() {
    return valid;
  }

  /**
   *
   * @return
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
