package br.com.fluentvalidator.handler;

import java.util.Collection;

public interface HandlerInvalidField<P> {

	Collection<Error> handle(final P object);

}
