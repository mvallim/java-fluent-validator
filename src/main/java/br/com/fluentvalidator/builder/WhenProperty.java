package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.Validator;

public interface WhenProperty<T, P> extends When<T, P, WhenProperty<T, P>> {

  /**
   *
   * @param validator
   * @return
   */
  WithValidator<T, P, WhenProperty<T, P>> withValidator(final Validator<P> validator);

}
