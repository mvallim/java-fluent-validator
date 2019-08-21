package br.com.fluentvalidator;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import br.com.fluentvalidator.builder.Validator;

public final class LazyInitializer {
	
	private static final Set<Validator<?>> cachedValidators = Collections.newSetFromMap(new ConcurrentHashMap<Validator<?>, Boolean>());
	
	private LazyInitializer() {
		super();
	}

	public static <T> Validator<T> initialize(final Validator<T> validator) {
		if (!cachedValidators.contains(validator)) {
			AbstractValidator.class.cast(validator).rules();
			cachedValidators.add(validator);
		}
		return validator;
	}

}
