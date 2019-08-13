package br.com.fluentvalidator.builder;

public interface WhenProperty<T, P> extends When<T, P> {

	WithValidator<T, P> withValidator(final Validator<P> validator);
	
}
