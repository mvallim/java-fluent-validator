package br.com.fluentvalidator;

public class Error {

	private final String message;

	private final String field;

	private final Object attemptedValue;
	
	private final String code;
	
	public static Error create(final String field, final String message, final String code, final Object attemptedValue) {
		return new Error(field, message, code, attemptedValue);
	}

	private Error(final String field, final String message, final String code, final Object attemptedValue) {
		this.field = field;
		this.message = message;
		this.code = code;
		this.attemptedValue = attemptedValue;
	}

	public String getField() {
		return this.field;
	}

	public String getMessage() {
		return this.message;
	}

	public String getCode() {
		return code;
	}

	public Object getAttemptedValue() {
		return this.attemptedValue;
	}

}
