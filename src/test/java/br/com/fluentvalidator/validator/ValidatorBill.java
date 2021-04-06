package br.com.fluentvalidator.validator;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Bill;
import br.com.fluentvalidator.predicate.LocalDatePredicate;

import java.time.LocalDate;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThan;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

public class ValidatorBill extends AbstractValidator<Bill> {

  @Override
  public void rules() {
    ruleFor(Bill::getDescription)
        .must(not(stringEmptyOrNull()))
          .withMessage("A description is required")
          .withFieldName("description")
          .withAttempedValue(Bill::getDescription);

    ruleFor(Bill::getValue)
        .must(not(nullValue()))
          .withMessage("A value must be provided")
          .withFieldName("value")
          .withAttempedValue(Bill::getValue)
          .critical()
        .must(greaterThan((float) 0))
          .withMessage("Bill value must be greather than 0")
          .withFieldName("value")
          .withAttempedValue(Bill::getValue);

    ruleFor(bill -> bill)
        .must(LocalDatePredicate.localDateAfterToday(Bill::getDueDate))
          .withMessage("Only future bills are allowed")
          .withFieldName("dueDate")
          .withAttempedValue(Bill::getDueDate)
          .critical()
        .must(LocalDatePredicate.localDateBetweenOrEqual(Bill::getDueDate, LocalDate.now(), LocalDate.now().plusYears(3)))
          .withMessage("Max due date is 3 years ahead")
          .withFieldName("dueDate")
          .withAttempedValue(Bill::getDueDate);
  }

}
