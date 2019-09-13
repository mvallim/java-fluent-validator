package br.com.fluentvalidator.spring.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import org.springframework.stereotype.Component;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Gender;
import br.com.fluentvalidator.model.Girl;

@Component
public class ValidatorSpringGirl extends AbstractValidator<Girl> {

	@Override
	protected void rules() {
		
		ruleFor(Girl::getGender)
			.must(equalTo(Gender.FEMALE))
				.when(not(nullValue()))			
				.withMessage("gender of girl must be FEMALE")
				.withFieldName("gender");
		
		ruleFor(Girl::getName)
			.must(stringContains("Ana"))
				.when(not(stringEmptyOrNull()))			
				.withMessage("child name must contains key Ana")
				.withFieldName("name");
	}

}
