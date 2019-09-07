package br.com.fluentvalidator.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Gender;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.predicate.PredicateBuilder;

public class ValidatorGirl extends AbstractValidator<Girl> {

	@Override
	protected void rules() {
		
		ruleFor(Girl::getGender)
			.when(PredicateBuilder.from(not(nullValue())))
				.must(PredicateBuilder.from(equalTo(Gender.FEMALE)))
				.withMessage("gender of girl must be FEMALE")
				.withFieldName("gender");
		
		ruleFor(Girl::getName)
			.when(PredicateBuilder.from(not(stringEmptyOrNull())))
				.must(PredicateBuilder.from(stringContains("Ana")))
				.withMessage("child name must contains key Ana")
				.withFieldName("name");
	}

}
