/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
