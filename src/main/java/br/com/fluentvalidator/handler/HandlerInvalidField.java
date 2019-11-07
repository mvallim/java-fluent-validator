package br.com.fluentvalidator.handler;

import java.util.Collection;
import java.util.Collections;
import br.com.fluentvalidator.context.Error;

public interface HandlerInvalidField<P> {

  default Collection<Error> handle(final P attemptedValue) {
    return Collections.emptyList();
  }

  default Collection<Error> handle(final Object instance, final P attemptedValue) {
    return handle(attemptedValue);
  }

}
