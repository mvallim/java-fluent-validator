package br.com.fluentvalidator.validator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Gender;

public class ValidatorBoy extends AbstractValidator<Boy>{

	@Override
	protected void rules() {
		
		setPropertyOnContext("boy");
			
		ruleFor(Boy::getGender)
			.when(gender -> notNullValue().matches(gender))
				.must(gender -> equalTo(Gender.MALE).matches(gender))
				.withMessage("gender of boy must be MALE")
				.withFieldName("gender")
				.critical();
		
		ruleFor(Boy::getName)
			.when(name -> not(isEmptyOrNullString()).matches(name))
				.must(name -> containsString("John").matches(name))
				.withMessage("child name must contains key John")
				.withFieldName("name");
	}

}
