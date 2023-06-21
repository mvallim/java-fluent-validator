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
