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
 * Utility class for managing validation context using thread-local storage.
 * <p>
 * Provides access to a thread-local {@link Context} instance, which holds validation properties and errors.
 * </p>
 *
 * <ul>
 *   <li>{@link #get()} - Retrieves the current thread's validation context, creating one if necessary.</li>
 *   <li>{@link #remove()} - Removes the validation context from the current thread.</li>
 * </ul>
 *
 * <p>
 * The nested {@link Context} class allows storing arbitrary properties and collecting validation errors.
 * </p>
 */
public final class ValidationContext {

  private static final ThreadLocal<Context> threadLocal = new ThreadLocal<>();

  /**
   * Private constructor to prevent instantiation of {@code ValidationContext}.
   * This ensures that the class can only be used in a static context or through controlled access.
   */
  private ValidationContext() {
    super();
  }

  /**
   * Retrieves the current {@link Context} instance associated with the calling thread.
   * If no instance exists, a new {@link Context} is created and associated with the thread.
   *
   * @return the {@link Context} instance for the current thread
   */
  public static Context get() {
    if (Objects.isNull(threadLocal.get())) {
      threadLocal.set(new Context());
    }
    return threadLocal.get();
  }

  /**
   * Removes the current {@link ValidationContext} instance from the thread-local storage.
   * This method should be called to clean up resources and avoid memory leaks
   * after validation operations are completed in the current thread.
   */
  public static void remove() {
    threadLocal.remove();
  }

  /**
   * Represents a validation context for storing properties and errors during validation.
   * <p>
   * This class maintains a thread-safe map of properties and a queue of validation errors.
   * It provides methods to add errors, set and retrieve properties, and obtain the validation result.
   * </p>
   *
   * <ul>
   *   <li>{@link #addErrors(Collection)} - Adds a collection of validation errors to the context.</li>
   *   <li>{@link #setProperty(String, Object)} - Sets a property in the context.</li>
   *   <li>{@link #getProperty(String, Class)} - Retrieves a property from the context, cast to the specified type.</li>
   *   <li>{@link #getValidationResult()} - Returns the validation result based on the collected errors.</li>
   * </ul>
   *
   * <p>
   * This class is intended for internal use within the validation framework.
   * </p>
   */
  public static final class Context implements AutoCloseable {

    private final Map<String, Object> properties = new ConcurrentHashMap<>();

    private final Queue<Error> errors = new ConcurrentLinkedQueue<>();

    /**
     * Adds a collection of {@link Error} objects to the current list of errors.
     *
     * @param errs the collection of errors to be added
     */
    public void addErrors(final Collection<Error> errs) {
      errs.stream().forEach(errors::add);
    }

    /**
     * Sets a property in the validation context with the specified key and value.
     * If the property key is not {@code null}, it will be added or updated in the context.
     *
     * @param property the key of the property to set; must not be {@code null}
     * @param value the value to associate with the property key
     */
    public void setProperty(final String property, final Object value) {
      if (Objects.nonNull(property)) {
        properties.put(property, value);
      }
    }

    /**
     * Retrieves the validation result for the current context.
     * <p>
     * This method clears the thread-local validation context and returns a {@link ValidationResult}
     * indicating whether validation errors were found. If no errors are present, {@link ValidationResult#ok()}
     * is returned; otherwise, {@link ValidationResult#fail(java.util.List)} is returned with the list of errors.
     * </p>
     *
     * @return the {@link ValidationResult} representing the outcome of the validation.
     */
    public ValidationResult getValidationResult() {
      ValidationContext.remove();
      return errors.isEmpty() ? ValidationResult.ok() : ValidationResult.fail(errors);
    }

    /**
     * Retrieves the value of a property by its name and casts it to the specified type.
     *
     * @param property the name of the property to retrieve
     * @param clazz the class object representing the desired return type
     * @param <P> the type of the property value
     * @return the property value cast to the specified type, or {@code null} if the property does not exist
     * @throws ClassCastException if the property value cannot be cast to the specified type
     */
    public <P> P getProperty(final String property, final Class<P> clazz) {
      return clazz.cast(properties.getOrDefault(property, null));
    }

    @Override
    public void close() {
      ValidationContext.remove();
    }
  }

}
