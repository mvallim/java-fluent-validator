package br.com.fluentvalidator;

public class Error {

	private final String message;

	private final String field;

	private final Object attemptedValue;

	public static Error create(final String field, final String message, final Object attemptedValue) {
		return new Error(field, message, attemptedValue);
	}

	private Error(final String field, final String message, final Object attemptedValue) {
		this.field = field;
		this.message = message;
		this.attemptedValue = attemptedValue;
	}

	public String getMessage() {
		return this.message;
	}

	public String getField() {
		return this.field;
	}

	public Object getAttemptedValue() {
		return this.attemptedValue;
	}

}
