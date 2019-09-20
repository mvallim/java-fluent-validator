package br.com.fluentvalidator.validator;

import static br.com.fluentvalidator.predicate.StringPredicate.stringMatches;

import br.com.fluentvalidator.AbstractValidator;

public class ValidatorId extends AbstractValidator<String> {

  private static final String UUID_REGEX = "[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}";

  @Override
  protected void rules() {

    setPropertyOnContext("id");

    ruleFor(id -> id).must(stringMatches(UUID_REGEX))
        .withMessage("id not matching the pattern of a UUID").withFieldName("id").critical();
  }

}
