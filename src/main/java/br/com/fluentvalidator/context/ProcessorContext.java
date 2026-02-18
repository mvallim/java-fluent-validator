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

import java.util.Deque;
import java.util.Objects;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.atomic.AtomicInteger;

public final class ProcessorContext {

  private static final ThreadLocal<Context> threadLocal = new ThreadLocal<>();

  /**
   * Private constructor to prevent instantiation of {@code ProcessorContext} from outside the class.
   * Ensures that instances can only be created internally, typically for singleton or factory patterns.
   */
  private ProcessorContext() {
    super();
  }

  /**
   * Retrieves the current {@link Context} instance associated with the calling thread.
   * If no {@link Context} exists for the thread, a new instance is created and set.
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
   * Removes the current thread's value for the processor context from the ThreadLocal storage.
   * This method should be called to clean up resources and avoid potential memory leaks
   * when the context is no longer needed in the current thread.
   */
  public static void remove() {
    threadLocal.remove();
  }

  /**
   * Context is a utility class that manages a stack of counters using {@link AtomicInteger}.
   * It provides methods to create, remove, increment, and retrieve the current counter value.
   * The stack is thread-safe, allowing concurrent access.
   *
   * <ul>
   *   <li>{@link #create()} - Pushes a new counter onto the stack, initialized to zero.</li>
   *   <li>{@link #remove()} - Removes the top counter from the stack if it exists.</li>
   *   <li>{@link #inc()} - Increments the top counter if the stack is not empty.</li>
   *   <li>{@link #get()} - Retrieves the value of the top counter, or zero if the stack is empty.</li>
   * </ul>
   */
  public static final class Context implements AutoCloseable {

    private final Deque<AtomicInteger> stackCounter = new ConcurrentLinkedDeque<>();

    /**
     * Initializes a new processing context by pushing a fresh {@link AtomicInteger} with value 0 onto the stack counter.
     * This method is typically used to start a new validation or processing scope.
     */
    public void create() {
      stackCounter.push(new AtomicInteger(0));
    }

    /**
     * Removes the top element from the {@code stackCounter} if it is not empty.
     * This method is typically used to manage the stack state within the processor context,
     * ensuring that elements are only removed when available.
     */
    public void remove() {
      if (!stackCounter.isEmpty()) {
        stackCounter.pop();
      }
    }

    /**
     * Increments the top value of the stack counter if the stack is not empty.
     * <p>
     * This method checks if the {@code stackCounter} is not empty and, if so,
     * increments the value at the top of the stack using {@code incrementAndGet()}.
     * </p>
     */
    public void inc() {
      if (!stackCounter.isEmpty()) {
        stackCounter.peek().incrementAndGet();
      }
    }

    /**
     * Retrieves the current value from the stack counter.
     * If the stack counter is empty, returns {@code 0}.
     *
     * @return the current value of the stack counter, or {@code 0} if empty
     */
    public Integer get() {
      return stackCounter.isEmpty() ? 0 : stackCounter.peek().get();
    }

    @Override
    public void close() {
      ProcessorContext.remove();
    }
  }

}
