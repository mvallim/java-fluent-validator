package br.com.fluentvalidator.handler;

import java.util.Collection;
import java.util.Collections;

import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.rule.FieldDescriptor;

public class DefaultHandlerInvalidField<P> implements HandlerInvalidField<P> {

	@Override
	public Collection<Error> handle(final FieldDescriptor fieldDescriptor, final P object) {
		return Collections.singletonList(Error.create(fieldDescriptor.getFieldName(), fieldDescriptor.getMessage(), fieldDescriptor.getCode(), object));
	}

}
