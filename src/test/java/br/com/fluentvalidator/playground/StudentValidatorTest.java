/*
 * Copyright 2023 the original author or authors.
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

package br.com.fluentvalidator.playground;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Before;
import org.junit.Test;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.playground.model.Student;
import br.com.fluentvalidator.playground.validator.StudentValidatorAnotherWay01;
import br.com.fluentvalidator.playground.validator.StudentValidatorAnotherWay02;
import br.com.fluentvalidator.playground.validator.StudentValidatorAnotherWay03;

public class StudentValidatorTest {

  private Student.Builder studentBuilder;
  private Student student;

  @Before
  public void setUp() throws Exception {
    studentBuilder = Student.Builder.newInstance();
  }

  @Test
  public void shouldValidateStudentValidatorAnotherWay01() {

    final Validator<Student> studentValidator = new StudentValidatorAnotherWay01();

    student = studentBuilder.build();
    final ValidationResult result = studentValidator.validate(student);
    assertThat(result.isValid()).isFalse();
  }

  @Test
  public void shouldValidateStudentValidatorAnotherWay02() {

    final Validator<Student> studentValidator = new StudentValidatorAnotherWay02();

    student = studentBuilder.build();
    final ValidationResult result = studentValidator.validate(student);
    assertThat(result.isValid()).isFalse();
  }

  @Test
  public void shouldValidateStudentValidatorAnotherWay03() {

    final Validator<Student> studentValidator = new StudentValidatorAnotherWay03();

    student = studentBuilder.build();
    final ValidationResult result = studentValidator.validate(student);
    assertThat(result.isValid()).isFalse();
  }

}
