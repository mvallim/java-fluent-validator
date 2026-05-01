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

package br.com.fluentvalidator.rule;

import java.util.function.Function;

import br.com.fluentvalidator.builder.AttemptedValue;
import br.com.fluentvalidator.builder.Code;
import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.HandleInvalidField;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.When;
import br.com.fluentvalidator.builder.Whenever;
import br.com.fluentvalidator.builder.WithValidator;

/**
 * Abstract base class for rule builders.
 * Provides common functionality for building validation rules with fluent API.
 *
 * @param <T> the type of object being validated
 * @param <P> the type of the property being validated
 * @param <W> the type of the When condition builder
 * @param <N> the type of the Whenever condition builder
 */
abstract class AbstractRuleBuilder<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>>
    implements Must<T, P, W, N>, Message<T, P, W, N>, FieldName<T, P, W, N>, Code<T, P, W, N>, Critical<T, P, W, N>, WithValidator<T, P, W, N>, HandleInvalidField<T, P, W, N>, AttemptedValue<T, P, W, N>, Rule<T> {

  protected final Function<T, String> fieldName;

  protected final Function<T, P> function;

  /**
   * Constructs a new AbstractRuleBuilder with a field name function and property function.
   *
   * @param fieldName the function to generate the field name
   * @param function the function to extract the property value
   */
  protected AbstractRuleBuilder(final Function<T, String> fieldName, final Function<T, P> function) {
    this.fieldName = fieldName;
    this.function = function;
  }

  /**
   * Constructs a new AbstractRuleBuilder with a static field name and property function.
   *
   * @param fieldName the static field name to use in error messages
   * @param function the function to extract the property value
   */
  protected AbstractRuleBuilder(final String fieldName, final Function<T, P> function) {
    this(obj -> fieldName, function);
  }

  /**
   * Constructs a new AbstractRuleBuilder with only a property function (no field name).
   *
   * @param function the function to extract the property value
   */
  protected AbstractRuleBuilder(final Function<T, P> function) {
    this(obj -> null, function);
  }

}
