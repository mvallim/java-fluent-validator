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

  private ProcessorContext() {
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
   * Context of processor
   */
  public static final class Context {

    private final Deque<AtomicInteger> stackCounter = new ConcurrentLinkedDeque<>();

    public void create() {
      stackCounter.push(new AtomicInteger(0));
    }

    public void remove() {
      if (!stackCounter.isEmpty()) {
        stackCounter.pop();
      }
    }

    public void inc() {
      if (!stackCounter.isEmpty()) {
        stackCounter.peek().incrementAndGet();
      }
    }

    public Integer get() {
      return stackCounter.isEmpty() ? 0 : stackCounter.peek().get();
    }

  }

}
