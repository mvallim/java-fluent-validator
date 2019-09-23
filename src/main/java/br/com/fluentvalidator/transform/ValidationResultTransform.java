package br.com.fluentvalidator.transform;

import br.com.fluentvalidator.context.ValidationResult;

public interface ValidationResultTransform<E> {

    /**
     *
     * @param validationResult
     * @return
     */
    E transform(final ValidationResult validationResult);

}
