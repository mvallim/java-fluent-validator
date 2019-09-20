package br.com.fluentvalidator.context;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.ArrayList;

import org.junit.Test;

import br.com.fluentvalidator.exception.ValidationSampleException;

public class ValidationResultTest {

  @Test
  public void testIsInvalidThrow() {
    assertThatThrownBy(() -> ValidationResult.fail(new ArrayList<>())
        .isInvalidThrow(ValidationSampleException.class))
            .isInstanceOf(ValidationSampleException.class);
    assertThatCode(() -> ValidationResult.ok().isInvalidThrow(ValidationSampleException.class))
        .doesNotThrowAnyException();
  }
}
