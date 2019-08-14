package br.com.fluentvalidator;

import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

public final class ValidationContext {

	private static final ThreadLocal<Context> threadLocal = new ThreadLocal<>();

	private ValidationContext() {
		super();
	}

	public static Context get() {
		if (threadLocal.get() == null) {
			threadLocal.set(new Context());
		}
		return threadLocal.get();
	}

	public static void remove() {
		threadLocal.remove();
	}

	public static final class Context {

		private final Map<String, Object> properties = new ConcurrentHashMap<>();

		private final Queue<Error> errors = new ConcurrentLinkedQueue<>();

		public void addError(final String field, final String message, final Object attemptedValue) {
			this.errors.add(Error.create(field, message, attemptedValue));
		}

		public void addProperty(final String property, final Object value) {
			this.properties.put(property, value);
		}

		public <P> P getProperty(final String property, final Class<P> clazz) {
			return clazz.cast(this.properties.getOrDefault(property, null));
		}

		public ValidationResult getValidationResult() {
			ValidationContext.remove();
			return this.errors.isEmpty() ? ValidationResult.ok() : ValidationResult.fail(this.errors);
		}
	}

}
