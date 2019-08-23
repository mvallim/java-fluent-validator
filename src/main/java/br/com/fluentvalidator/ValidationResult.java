package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import br.com.fluentvalidator.exception.Error;

public class ValidationResult {

	private final boolean valid;

	private final Collection<Error> errors;

	public static ValidationResult ok() {
		return new ValidationResult(true, new ArrayList<>());
	}

	public static ValidationResult fail(final Collection<Error> messages) {
		return new ValidationResult(false, messages);
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
}