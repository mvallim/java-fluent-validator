package br.com.fluentvalidator.exception;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.ValidationResult;

public abstract class ValidationException extends RuntimeException {

	private static final long serialVersionUID = 2274879814700248645L;

	private final transient ValidationResult validationResult;

	protected ValidationException(final ValidationResult validationResult) {
		super(validationResult.toString());
		this.validationResult = validationResult;
	}

	public ValidationResult getValidationResult() {
		return validationResult;
	}

	public static RuntimeException create(final Class<? extends ValidationException> exceptionClass) {
		try {
			final ValidationResult validationResult = ValidationContext.get().getValidationResult();
			final Constructor<? extends ValidationException> ctor = exceptionClass.getConstructor(ValidationResult.class);
			return ctor.newInstance(validationResult);
		} catch (final NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			return new RuntimeException("Constructor in class not found (ValidationResult validationResult)", e);
		}
	}

}
