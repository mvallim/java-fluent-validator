package br.com.fluentvalidator.builder;

import java.util.Collection;

public interface WhenCollection<T, P> extends When<T, Collection<P>> {

	WithValidator<T, Collection<P>> withValidator(final Validator<P> validator);
	
}
