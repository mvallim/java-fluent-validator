package br.com.fluentvalidator.aspect;

import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;

import br.com.fluentvalidator.annotation.CleanValidationContextException;
import br.com.fluentvalidator.context.ValidationContext;

@Aspect
public class ValidationExceptionAdvice {

  @AfterThrowing("@annotation(cleanValidationContextException)")
  public void afterThrowing(final CleanValidationContextException cleanValidationContextException) throws Throwable {
    ValidationContext.remove();
  }

}
