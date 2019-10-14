package br.com.fluentvalidator.handler;

import java.util.Collection;

import br.com.fluentvalidator.context.Error;

public interface HandlerInvalidField<P> {

	Collection<Error> handle(final P object);

}
