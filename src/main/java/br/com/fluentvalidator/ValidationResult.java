package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;

import br.com.fluentvalidator.exception.Error;

public final class ValidationResult {

	private final boolean valid;

	private final Collection<Error> errors;

	static ValidationResult ok() {
		return new ValidationResult(true, new ArrayList<>());
	}

	static ValidationResult fail(final Collection<Error> messages) {
		return new ValidationResult(false, Optional.ofNullable(messages).orElse(new ArrayList<>()));
	}

	private ValidationResult(final boolean valid, final Collection<Error> messages) {
		this.valid = valid;
		this.errors = Collections.unmodifiableCollection(messages);
	}

	public boolean isValid() {
		return this.valid;
	}

	public Collection<Error> getErrors() {
		return this.errors;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ValidationResult [valid=");
		builder.append(valid);
		builder.append(", ");
		builder.append("errors=");
		builder.append(errors);
		builder.append("]");
		return builder.toString();
	}
	
	
	
}