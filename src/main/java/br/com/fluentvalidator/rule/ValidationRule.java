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
import java.util.function.Predicate;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.ValidationException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

interface ValidationRule<T, P> extends Rule<P> {

  void when(final Predicate<P> when);

  void must(final Predicate<P> must);

  void withFieldName(final Function<?, String> fieldName);

  void withMessage(final Function<?, String> message);

  void withCode(final Function<?, String> code);

  void withAttemptedValue(final Function<?, Object> attemptedValue);

  void withHandlerInvalidField(final HandlerInvalidField<P> handleInvalid);

  void critical();

  void critical(final Class<? extends ValidationException> clazz);

  void whenever(final Predicate<P> whenever);

  void withValidator(final Validator<T> validator);

}
