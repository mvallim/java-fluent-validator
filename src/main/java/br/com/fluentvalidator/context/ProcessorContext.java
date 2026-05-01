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

/**
 * Manages the thread-local processor context for tracking validation counters.
 * Used to count the number of validations performed during the validation process.
 */
public final class ProcessorContext {

  private static final ThreadLocal<Context> threadLocal = new ThreadLocal<>();

  /**
   * Private constructor to prevent instantiation.
   */
  private ProcessorContext() {
    super();
  }

  /**
   * Returns the current processor context for this thread.
   * Creates a new context if one does not exist.
   *
   * @return the current processor context
   */
  public static Context get() {
    if (Objects.isNull(threadLocal.get())) {
      threadLocal.set(new Context());
    }
    return threadLocal.get();
  }

  /**
   * Removes the processor context for this thread.
   */
  public static void remove() {
    threadLocal.remove();
  }

  /**
   * The processor context that maintains a stack of counters for tracking validations.
   * Implements AutoCloseable for use with try-with-resources.
   */
  public static final class Context implements AutoCloseable {

    private final Deque<AtomicInteger> stackCounter = new ConcurrentLinkedDeque<>();

    /**
     * Creates a new counter and pushes it onto the stack.
     */
    public void create() {
      stackCounter.push(new AtomicInteger(0));
    }

    /**
     * Removes the current counter from the stack.
     */
    public void remove() {
      if (!stackCounter.isEmpty()) {
        stackCounter.pop();
      }
    }

    /**
     * Increments the current counter in the stack.
     */
    public void inc() {
      if (!stackCounter.isEmpty()) {
        stackCounter.peek().incrementAndGet();
      }
    }

    /**
     * Returns the current counter value.
     *
     * @return the current counter value, or 0 if the stack is empty
     */
    public Integer get() {
      return stackCounter.isEmpty() ? 0 : stackCounter.peek().get();
    }

    /**
     * Closes the context and removes it from the thread-local storage.
     */
    @Override
    public void close() {
      ProcessorContext.remove();
    }
  }

}
