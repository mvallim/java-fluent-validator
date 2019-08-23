package br.com.fluentvalidator.exception;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Collections;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.ValidationResult;

public abstract class ValidationException extends RuntimeException {

	private static final long serialVersionUID = 2274879814700248645L;

	private final transient Collection<Error> errors;

	protected ValidationException(final Collection<Error> errors) {
		super(errors.toString());
		this.errors = errors;
	}

	public Collection<Error> getErrors() {
		return Collections.unmodifiableCollection(this.errors);
	}

	public static RuntimeException create(final Class<? extends ValidationException> exceptionClass) {
		try {
			final ValidationResult validationResult = ValidationContext.get().getValidationResult();
			final Collection<Error> parameter = validationResult.getErrors();
			final Constructor<? extends ValidationException> ctor = exceptionClass.getConstructor(Collection.class);
			return ctor.newInstance(parameter);
		} catch (final NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			return new RuntimeException("Constructor in class not found (Collection<Error> errors)", e);
		}
	}

}
