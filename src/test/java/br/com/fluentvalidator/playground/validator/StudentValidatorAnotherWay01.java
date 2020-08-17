package br.com.fluentvalidator.playground.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.playground.model.Student;

// @formatter:off
public class StudentValidatorAnotherWay01 extends AbstractValidator<Student> {

  @Override
  public void rules() {

    ruleFor(n -> n)
      .must(not(stringEmptyOrNull(n -> n.getId())))
        .when(stringEmptyOrNull(n -> n.getEnrolmentId()))
        .withMessage("Enrolment Id is null, Id must not be null");

    ruleFor(n -> n)
      .must(not(stringEmptyOrNull(n -> n.getEnrolmentId())))
      .when(stringEmptyOrNull(n -> n.getId()))
        .withMessage("Id is null, Enrolment Id must not be null");

  }

}
// @formatter:on
