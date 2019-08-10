package br.com.fluentvalidator;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

final class ValidationContext {

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

	static class Context {

		private final Queue<Error> errors = new ConcurrentLinkedQueue<>();

		public void addError(final String field, final String message, final Object attemptedValue) {
			this.errors.add(Error.create(field, message, attemptedValue));
		}

		public ValidationResult getValidationResult() {
			ValidationContext.remove();
			return this.errors.isEmpty() ? ValidationResult.ok() : ValidationResult.fail(this.errors);
		}
	}

}
