package br.com.fluentvalidator.exception;

import java.util.Collection;

public class ValidationSampleException extends ValidationException {

	private static final long serialVersionUID = -8340774064473719970L;

	public ValidationSampleException(final Collection<Error> errors) {
		super(errors);
	}

}
