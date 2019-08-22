package br.com.fluentvalidator.spring.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.StringPredicate.matches;

import org.springframework.stereotype.Component;

import br.com.fluentvalidator.AbstractValidator;

@Component
public class ValidatorSpringId extends AbstractValidator<String>{

	private static final String UUID_REGEX = "[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}";
	
	@Override
	protected void rules() {
		
		setPropertyOnContext("id");
			
		ruleFor(id -> id)
			.when(isTrue())
				.must(matches(UUID_REGEX))
				.withMessage("id not matching the pattern of a UUID")
				.withFieldName("id")
				.critical();
	}

}
