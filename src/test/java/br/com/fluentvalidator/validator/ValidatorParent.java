package br.com.fluentvalidator.validator;

import static br.com.fluentvalidator.predicate.CollectionPredicate.empty;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

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
			.when(isTrue())
				.must(not(nullValue()))
				.withMessage("parent's children cannot be null")
				.withFieldName("children")
			.when(not(nullValue()))
				.must(not(empty()))
				.withMessage("parent must have at least one child")
				.withFieldName("children")		
			.when(not(nullValue()))
				.withValidator(new ValidatorChild())
				.critical();		
	
		ruleFor(Parent::getId)
			.when(isTrue())
				.withValidator(new ValidatorId())
				.critical();

		ruleFor(Parent::getAge)
			.when(not(nullValue()))
				.must(greaterThanOrEqual(5))
				.withMessage("age must be greater than or equal to 10")
				.withFieldName("age")
			.when(not(nullValue()))
				.must(lessThanOrEqual(7))
				.withMessage("age must be less than or equal to 7")
				.withFieldName("age");

		ruleFor(Parent::getCities)
			.when(not(nullValue()))
				.must(hasSize(10))
				.withMessage("cities size must be 10")
				.withFieldName("cities");

		ruleFor(Parent::getName)
			.when(not(stringEmptyOrNull()))
				.must(stringContains("John"))
				.withMessage("name must contains key John")
				.withFieldName("name");

		ruleForEach(parent -> extractGirls(parent.getChildren()))
			.when(not(nullValue()))
				.withValidator(new ValidatorGirl());
		
		ruleForEach(parent -> extractBoys(parent.getChildren()))
			.when(not(nullValue()))
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
