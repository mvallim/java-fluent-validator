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

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * Manages the thread-local validation context.
 * The context holds validation errors and properties during the validation process.
 */
public final class ValidationContext {

  private static final ThreadLocal<Context> threadLocal = new ThreadLocal<>();

  /**
   * Private constructor to prevent instantiation.
   */
  private ValidationContext() {
    super();
  }

  /**
   * Returns the current validation context for this thread.
   * Creates a new context if one does not exist.
   *
   * @return the current validation context
   */
  public static Context get() {
    if (Objects.isNull(threadLocal.get())) {
      threadLocal.set(new Context());
    }
    return threadLocal.get();
  }

  /**
   * Removes the validation context for this thread.
   */
  public static void remove() {
    threadLocal.remove();
  }

  /**
   * The validation context that holds properties and collects validation errors.
   * Implements AutoCloseable for use with try-with-resources.
   */
  public static final class Context implements AutoCloseable {

    private final Map<String, Object> properties = new ConcurrentHashMap<>();

    private final Queue<Error> errors = new ConcurrentLinkedQueue<>();

    /**
     * Adds a collection of errors to the context.
     *
     * @param errs the collection of errors to add
     */
    public void addErrors(final Collection<Error> errs) {
      errs.stream().forEach(errors::add);
    }

    /**
     * Sets a property in the context for use during validation.
     *
     * @param property the name of the property
     * @param value the value of the property
     */
    public void setProperty(final String property, final Object value) {
      if (Objects.nonNull(property)) {
        properties.put(property, value);
      }
    }

    /**
     * Returns the validation result based on the collected errors.
     * Removes the context after generating the result.
     *
     * @return a ValidationResult indicating success or failure with errors
     */
    public ValidationResult getValidationResult() {
      ValidationContext.remove();
      return errors.isEmpty() ? ValidationResult.ok() : ValidationResult.fail(errors);
    }

    /**
     * Retrieves a property from the context.
     *
     * @param property the name of the property
     * @param clazz the expected class of the property value
     * @param <P> the type of the property value
     * @return the property value, or null if not found
     */
    public <P> P getProperty(final String property, final Class<P> clazz) {
      return clazz.cast(properties.getOrDefault(property, null));
    }

    /**
     * Closes the context and removes it from the thread-local storage.
     */
    @Override
    public void close() {
      ValidationContext.remove();
    }
  }

}
