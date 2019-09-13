package br.com.fluentvalidator.exception;

import org.junit.Test;

public class ValidationExceptionTest {
	
	@Test(expected = ValidationSampleException.class)
	public void testSuccess() {
		
		throw ValidationException.create(ValidationSampleException.class);
		
	}
	
	@Test(expected = RuntimeException.class)
	public void testFail() {
		
		throw ValidationException.create(ValidationSampleInvalidException.class);
		
	}

}
