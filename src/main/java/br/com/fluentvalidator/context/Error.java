package br.com.fluentvalidator.context;

public class Error {

	private final String message;

	private final String field;

	private final Object attemptedValue;

	private final String code;

	public static Error create(final String field, final String message, final String code, final Object attemptedValue) {
		return new Error(field, message, code, attemptedValue);
	}

	protected Error(final String field, final String message, final String code, final Object attemptedValue) {
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
		return this.code;
	}

	public Object getAttemptedValue() {
		return this.attemptedValue;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();

		builder.append("Error [");
		builder.append("message=");
		builder.append(message);
		builder.append(", ");
		builder.append("field=");
		builder.append(field);
		builder.append(", ");
		builder.append("attemptedValue=");
		builder.append(attemptedValue);
		builder.append(", ");
		builder.append("code=");
		builder.append(code);
		builder.append("]");

		return builder.toString();
	}

}
