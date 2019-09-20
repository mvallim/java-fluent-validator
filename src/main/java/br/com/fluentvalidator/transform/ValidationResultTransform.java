package br.com.fluentvalidator.transform;

import br.com.fluentvalidator.context.ValidationResult;

public interface ValidationResultTransform<E> {

  E transform(final ValidationResult validationResult);

}
