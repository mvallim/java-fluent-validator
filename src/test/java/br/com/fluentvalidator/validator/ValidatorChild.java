package br.com.fluentvalidator.validator;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Child;
import br.com.fluentvalidator.model.Parent;

public class ValidatorChild extends AbstractValidator<Child>{

	@Override
	protected void rules() {
		
		setPropertyOnContext("child");
		
		ruleFor(Child::getAge)
			.when(age -> true)
				.must(age -> notNullValue().matches(age))
				.withMessage("child age must be not null")
				.withFieldName("age")
			.when(age -> true)
				.must(age -> greaterThanOrEqualTo(5).matches(age))
				.withMessage("child age must be greater than or equal to 5")
				.withFieldName("age")
			.when(age -> true)
				.must(this::checkAgeConstraintChild)
				.withMessage("child age must be less than age parent")
				.withFieldName("age");
			
		ruleFor(Child::getName)
			.when(name -> true)
				.must(name -> not(isEmptyOrNullString()).matches(name))
				.withMessage("child name must be not null or empty")
				.withFieldName("name")
			.when(name -> true)
				.must(name -> containsString("John").matches(name))
				.withMessage("child name must contains key John")
				.withFieldName("name");
		
	}
	
	private boolean checkAgeConstraintChild(final Integer age) {
		return age < getPropertyOnContext("parent", Parent.class).getAge();
	}

}
