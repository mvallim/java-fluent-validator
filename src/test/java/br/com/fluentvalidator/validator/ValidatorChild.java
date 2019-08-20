package br.com.fluentvalidator.validator;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Child;
import br.com.fluentvalidator.model.Parent;

public class ValidatorChild extends AbstractValidator<Child>{

	@Override
	protected void rules() {
		
		setPropertyOnContext("child");
		
		ruleFor(Child::getAge)
			.when(isTrue())
				.must(not(nullValue()))
				.withMessage("child age must be not null")
				.withFieldName("age")
			.when(isTrue())
				.must(greaterThanOrEqual(5))
				.withMessage("child age must be greater than or equal to 5")
				.withFieldName("age")
			.when(isTrue())
				.must(this::checkAgeConstraintChild)
				.withMessage("child age must be less than age parent")
				.withFieldName("age")
				.critical();
			
		ruleFor(Child::getName)
			.when(isTrue())
				.must(not(stringEmptyOrNull()))
				.withMessage("child name must be not null or empty")
				.withFieldName("name");

	}
	
	private boolean checkAgeConstraintChild(final Integer age) {
		return age < getPropertyOnContext("parent", Parent.class).getAge();
	}

}
