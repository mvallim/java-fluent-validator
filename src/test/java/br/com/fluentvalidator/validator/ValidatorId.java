package br.com.fluentvalidator.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringMatches;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.predicate.PredicateBuilder;

public class ValidatorId extends AbstractValidator<String>{

	private static final String UUID_REGEX = "[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}";
	
	@Override
	protected void rules() {
		
		setPropertyOnContext("id");
			
		ruleFor(id -> id)
			.when(PredicateBuilder.from(isTrue()))
				.must(stringMatches(UUID_REGEX))
				.withMessage("id not matching the pattern of a UUID")
				.withFieldName("id")
				.critical();
	}

}
