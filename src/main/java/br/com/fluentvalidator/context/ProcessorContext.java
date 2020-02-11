package br.com.fluentvalidator.context;

import java.util.Objects;
import java.util.Stack;
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

    private final Stack<AtomicInteger> stackCounter = new Stack<>();

    public void create() {
      stackCounter.add(new AtomicInteger(0));
    }

    public void remove() {
      if (!stackCounter.isEmpty()) {
        stackCounter.remove(stackCounter.firstElement());
      }
    }

    public void inc() {
      if (!stackCounter.isEmpty()) {
        stackCounter.firstElement().incrementAndGet();
      }
    }

    public Integer get() {
      return stackCounter.isEmpty() ? 0 : stackCounter.firstElement().get();
    }

  }

}
