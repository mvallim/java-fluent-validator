package br.com.fluentvalidator.validator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Gender;
import br.com.fluentvalidator.model.Girl;

public class ValidatorGirl extends AbstractValidator<Girl>{

	@Override
	protected void rules() {
		
		setPropertyOnContext("girl");
			
		ruleFor(Girl::getGender)
			.when(gender -> notNullValue().matches(gender))
				.must(gender -> equalTo(Gender.FEMALE).matches(gender))
				.withMessage("gender of girl must be FEMALE")
				.withFieldName("gender");
		
		ruleFor(Girl::getName)
			.when(name -> not(isEmptyOrNullString()).matches(name))
				.must(name -> containsString("Ana").matches(name))
				.withMessage("child name must contains key Ana")
				.withFieldName("name");
	}

}
