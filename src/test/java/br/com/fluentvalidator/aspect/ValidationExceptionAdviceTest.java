package br.com.fluentvalidator.aspect;

import static org.assertj.core.api.Assertions.catchThrowableOfType;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationContext.Context;
import br.com.fluentvalidator.context.ValidationResult;

// @formatter:off
public class ValidationExceptionAdviceTest {

  @Test
  public void validationMustBeSuccess() {
    final Validator<String> validatorParent = new ValidatorExceptionParent();

    final ValidationResult result = validatorParent.validate("123");

    assertTrue(result.isValid());
  }

  @Test
  public void validationMustBeFail() {
    final Validator<String> validatorParent = new ValidatorExceptionParent();

    final Context contextBefore = ValidationContext.get();

    validatorParent.validate("456");
    catchThrowableOfType(() -> validatorParent.validate("321"), RuntimeException.class);

    final Context contextAfter = ValidationContext.get();

    assertThat(contextBefore, not(equalTo(contextAfter)));
    assertThat(contextAfter.getValidationResult().isValid(), equalTo(true));
  }

  public class ValidatorExceptionParent extends AbstractValidator<String> {

    @Override
    public void rules() {
      ruleFor(str -> str)
        .must(str -> {
          if (str == "123") {
            return true;
          }
          if (str == "456") {
            return false;
          }
          throw new RuntimeException();
        });
    }

  }

}
// @formatter:on
