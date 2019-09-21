package br.com.fluentvalidator.builder;

import java.util.Collection;

import br.com.fluentvalidator.Validator;

public interface WhenCollection<T, P> extends When<T, Collection<P>, WhenCollection<T, P>> {

  /**
   *
   * @param validator
   * @return
   */
  WithValidator<T, Collection<P>, WhenCollection<T, P>> withValidator(final Validator<P> validator);

}
