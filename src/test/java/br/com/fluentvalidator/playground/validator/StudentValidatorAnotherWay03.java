package br.com.fluentvalidator.playground.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.playground.model.Student;

// @formatter:off
public class StudentValidatorAnotherWay03 extends AbstractValidator<Student> {

  @Override
  public void rules() {

    ruleFor(root -> root)
      .must(not(stringEmptyOrNull(Student::getId)).and(not(stringEmptyOrNull(Student::getEnrolmentId))))
        .withMessage("Enrolment Id is null and Id is null, please inform any one");

  }

}
// @formatter:on
