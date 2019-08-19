package br.com.fluentvalidator.validator;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Child;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.model.Parent;

public class ValidatorParent extends AbstractValidator<Parent> {

	@Override
	protected void rules() {
		
		setPropertyOnContext("parent");
		
		ruleForEach(Parent::getChildren)
			.when(children -> true)
				.must(children -> notNullValue().matches(children))
				.withMessage("parent's children cannot be null")
				.withFieldName("children")
			.when(children -> true)
				.must(children -> not(empty()).matches(children))
				.withMessage("parent must have at least one child")
				.withFieldName("children")		
			.when(children -> notNullValue().matches(children))
				.withValidator(new ValidatorChild())
				.critical();		
	
		ruleFor(Parent::getId)
			.when(id -> true)
				.withValidator(new ValidatorId())
				.critical();

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

		ruleForEach(parent -> extractGirls(parent.getChildren()))
			.when(girls -> notNullValue().matches(girls))
				.withValidator(new ValidatorGirl());
		
		ruleForEach(parent -> extractBoys(parent.getChildren()))
			.when(boys -> notNullValue().matches(boys))
				.withValidator(new ValidatorBoy())
				.critical();

	}
	
	private Collection<Girl> extractGirls(Collection<Child> children) {
		return Optional.ofNullable(children).orElseGet(ArrayList::new)
				.stream()
				.filter(Girl.class::isInstance)
				.map(Girl.class::cast)
				.collect(Collectors.toList());
	}

	private Collection<Boy> extractBoys(Collection<Child> children) {
		return Optional.ofNullable(children).orElseGet(ArrayList::new)
				.stream()
				.filter(Boy.class::isInstance)
				.map(Boy.class::cast)
				.collect(Collectors.toList());
	}

}
