package br.com.fluentvalidator.validator;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.*;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Parent;

public class ValidatorParent extends AbstractValidator<Parent> {

	@Override
	protected void rules() {
		
		setPropertyOnContext("parent");

		ruleFor(Parent::getAge)
			.when(age -> notNullValue().matches(age))
				.must(age -> greaterThanOrEqualTo(5).matches(age))
				.withMessage("age must be greater than or equal to 10")
				.withFieldName("age")
			.when(age -> notNullValue().matches(age))
				.must(age -> lessThanOrEqualTo(7).matches(age))
				.withMessage("age must be less than or equal to 7")
				.withFieldName("age");

		ruleFor(Parent::getCities)
			.when(cities -> notNullValue().matches(cities))
				.must(cities -> hasSize(10).matches(cities))
				.withMessage("cities size must be 10")
				.withFieldName("cities");

		ruleFor(Parent::getName)
			.when(name -> not(isEmptyOrNullString()).matches(name))
				.must(name -> containsString("John").matches(name))
				.withMessage("name must contains key John")
				.withFieldName("name");
		
		ruleFor(Parent::getChildren)
			.when(children -> true)
				.must(children -> notNullValue().matches(children))
				.withMessage("parent's children cannot be null")
				.withFieldName("children")
			.when(children -> true)
				.must(children -> not(empty()).matches(children))
				.withMessage("parent must have at least one child")
				.withFieldName("children");
				
		ruleForEach(Parent::getChildren)
			.when(children -> notNullValue().matches(children))
				.withValidator(new ValidatorChild());

	}

}
