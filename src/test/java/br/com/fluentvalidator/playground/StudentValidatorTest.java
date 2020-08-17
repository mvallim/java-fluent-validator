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
