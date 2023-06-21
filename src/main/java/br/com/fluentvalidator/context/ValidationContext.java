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

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

public final class ValidationContext {

  private static final ThreadLocal<Context> threadLocal = new ThreadLocal<>();

  private ValidationContext() {
    super();
  }

  /**
   *
   * @return
   */
  public static Context get() {
    if (Objects.isNull(threadLocal.get())) {
      threadLocal.set(new Context());
    }
    return threadLocal.get();
  }

  /**
   *
   */
  public static void remove() {
    threadLocal.remove();
  }

  /**
   * Context of validation
   */
  public static final class Context {

    private final Map<String, Object> properties = new ConcurrentHashMap<>();

    private final Queue<Error> errors = new ConcurrentLinkedQueue<>();

    /**
     *
     * @param field
     * @param message
     * @param code
     * @param attemptedValue
     */
    public void addErrors(final Collection<Error> errs) {
      errs.stream().forEach(errors::add);
    }

    /**
     *
     * @param property
     * @param value
     */
    public void setProperty(final String property, final Object value) {
      if (Objects.nonNull(property)) {
        properties.put(property, value);
      }
    }

    /**
     *
     * @return
     */
    public ValidationResult getValidationResult() {
      ValidationContext.remove();
      return errors.isEmpty() ? ValidationResult.ok() : ValidationResult.fail(errors);
    }

    /**
     *
     * @param property
     * @param clazz
     * @return
     */
    public <P> P getProperty(final String property, final Class<P> clazz) {
      return clazz.cast(properties.getOrDefault(property, null));
    }

  }

}
