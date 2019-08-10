package br.com.fluentvalidator.validator;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.*;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Parent;

public class ValidatorParent extends AbstractValidator<Parent> {

	@Override
	protected void rules() {

		ruleFor(Parent::getAge)
			.when(age -> notNullValue().matches(age))
			.must(age -> greaterThanOrEqualTo(5).matches(age))
			.withFieldName("age")
			.withMessage("age must be greater than or equal to 10")
			.must(age -> lessThanOrEqualTo(7).matches(age))
			.withFieldName("age")
			.withMessage("age must be less than or equal to 7");

		ruleFor(Parent::getCities)
			.when(cities -> notNullValue().matches(cities))
			.must(cities -> hasSize(10).matches(cities))
			.withFieldName("cities")
			.withMessage("cities size must be 10");

		ruleFor(Parent::getName)
			.when(name -> not(isEmptyOrNullString()).matches(name))
			.must(name -> containsString("John").matches(name))
			.withFieldName("name")
			.withMessage("name must contains key John");
		
		ruleFor(Parent::getChildren)
			.when(children -> true)
			.must(children -> notNullValue().matches(children))
			.withFieldName("children")
			.withMessage("parent's children cannot be null")			
			.must(children -> not(empty()).matches(children))
			.withFieldName("children")
			.withMessage("parent must have at least one child");
		
		ruleForEach(Parent::getChildren)
			.when(children -> notNullValue().matches(children))
			.withValidator(new ValidatorChild());

	}

}
