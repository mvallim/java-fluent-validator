package br.com.fluentvalidator.handler;

import java.util.Collection;

import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.rule.FieldDescriptor;

public interface HandlerInvalidField<P> {

	Collection<Error> handle(final FieldDescriptor fieldDescriptor, final P object);

}
