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
      stackCounter.add(new AtomicInteger(0));
    }

    public void remove() {
      if (!stackCounter.isEmpty()) {
        stackCounter.removeFirst();
      }
    }

    public void inc() {
      if (!stackCounter.isEmpty()) {
        stackCounter.getFirst().incrementAndGet();
      }
    }

    public Integer get() {
      return stackCounter.isEmpty() ? 0 : stackCounter.getFirst().get();
    }

  }

}
