package br.com.fluentvalidator.rule;

import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.function.Predicate;

import org.junit.After;
import org.junit.Test;

import br.com.fluentvalidator.ValidationContext;

public class ValidationRuleContainerTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}

	@Test
	public void testValidationRuleContainer() {
		
		final ValidationRuleContainer<String, String> validationRuleContainer = new ValidationRuleContainer<>();
		
		final Validation<String, String> validation = new StringValidationRule(when -> true);
		
		validationRuleContainer.addRule(validation);
		
		assertThat(validationRuleContainer.getCurrentValidation(), not(nullValue()));
		assertThat(validationRuleContainer.getCurrentValidation(), equalTo(validation));
		assertThat(validationRuleContainer.getRules(), not(nullValue()));
		assertThat(validationRuleContainer.getRules(), not(empty()));
		assertThat(validationRuleContainer.getRules(), hasSize(1));
	}
	
	class StringValidationRule extends ValidationRule<String, String> {

		protected StringValidationRule(Predicate<String> when) {
			super(when);
		}

		@Override
		boolean accept(String instance) {
			return false;
		}
		
	}

}
