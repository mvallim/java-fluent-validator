package br.com.fluentvalidator.context;

import java.util.Objects;

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

}
