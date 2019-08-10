package br.com.fluentvalidator;

public interface ValidationBuilder<P> extends Validation<P> {

	Validation<P> withValidator(final Validator<P> validator);

}
