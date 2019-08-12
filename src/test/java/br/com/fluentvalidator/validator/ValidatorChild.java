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
			.withFieldName("age")
			.withMessage("child age must be not null")
			.must(age -> greaterThanOrEqualTo(5).matches(age))
			.withFieldName("age")
			.withMessage("child age must be greater than or equal to 5")
			.must(this::checkAgeConstraintChild)
			.withFieldName("age")
			.withMessage("child age must be less than age parent");
			

		ruleFor(Child::getName)
			.when(name -> true)
			.must(name -> not(isEmptyOrNullString()).matches(name))
			.withFieldName("name")
			.withMessage("child name must be not null or empty")
			.must(name -> containsString("John").matches(name))
			.withFieldName("name")
			.withMessage("child name must contains key John");
		
	}
	
	private boolean checkAgeConstraintChild(final Integer age) {
		return age < getPropertyOnContext("parent", Parent.class).getAge();
	}

}
