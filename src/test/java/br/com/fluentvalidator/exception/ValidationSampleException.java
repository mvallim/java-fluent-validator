package br.com.fluentvalidator.exception;

import br.com.fluentvalidator.context.ValidationResult;

public class ValidationSampleException extends ValidationException {

  private static final long serialVersionUID = -8340774064473719970L;

  public ValidationSampleException(final ValidationResult validationResult) {
    super(validationResult);
  }

}
