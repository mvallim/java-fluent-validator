package br.com.fluentvalidator.exception;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.Test;

public class ValidationExceptionTest {

    @Test
    public void testSuccess() {

        assertThatThrownBy(() -> {
            throw ValidationException.create(ValidationSampleException.class);
        }).isInstanceOf(ValidationSampleException.class);

    }

    @Test
    public void testFail() {

        assertThatThrownBy(() -> {
            throw ValidationException.create(ValidationSampleInvalidException.class);
        }).isInstanceOf(RuntimeException.class);

    }

}
