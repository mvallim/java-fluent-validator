package br.com.fluentvalidator.playground.validator;

import static br.com.fluentvalidator.function.FunctionBuilder.of;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.playground.model.Student;

// @formatter:off
public class StudentValidatorAnotherWay02 extends AbstractValidator<Student> {

  @Override
  public void rules() {

    ruleFor(root -> root)
      .must(not(stringEmptyOrNull(Student::getId)))
      .when(stringEmptyOrNull(of(Student::getEnrolmentId)))
        .withMessage("Enrolment Id is null, Id must not be null");

    ruleFor(root -> root)
      .must(not(stringEmptyOrNull(Student::getEnrolmentId)))
      .when(stringEmptyOrNull(of(Student::getId)))
        .withMessage("Id is null, Enrolment Id must not be null");

  }

}
// @formatter:on
