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

package br.com.fluentvalidator.aspect;

import static br.com.fluentvalidator.predicate.CollectionPredicate.empty;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEquals;
import static org.assertj.core.api.Assertions.catchThrowableOfType;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationContext.Context;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.predicate.PredicateBuilder;

// @formatter:off
public class ValidationExceptionAdviceTest {

  @Test
  public void validationMustBeSuccess() {
    final Validator<ObjectFrom> validatorParent = new ValidatorObjectFrom();

    final ObjectFrom instance = new ObjectFrom();
    instance.setValue("123");
    instance.setValues(Arrays.asList("123"));

    final ValidationResult result = validatorParent.validate(instance);

    assertTrue(result.isValid());
  }

  @Test
  public void validationMustBeFail() {
    final Validator<ObjectFrom> validatorParent = new ValidatorObjectFrom();

    final Context contextBefore = ValidationContext.get();

    final ObjectFrom instance = new ObjectFrom();
    instance.setValue("111");
    instance.setValues(Arrays.asList("321"));

    catchThrowableOfType(() -> validatorParent.validate(instance), RuntimeException.class);

    final Context contextAfter = ValidationContext.get();

    assertThat(contextBefore, org.hamcrest.CoreMatchers.not(equalTo(contextAfter)));
    assertThat(contextAfter.getValidationResult().isValid(), equalTo(true));
  }

  public class ValidatorObjectFrom extends AbstractValidator<ObjectFrom> {

    @Override
    public void rules() {

      ruleFor(ObjectFrom::getValue)
        .whenever(not(stringEmptyOrNull()))
          .withValidator(new ValidatorException());

      ruleForEach(ObjectFrom::getValues)
        .whenever(not(empty()))
          .withValidator(new ValidatorException());

    }

  }

  public class ValidatorException extends AbstractValidator<String> {

    @Override
    public void rules() {

      ruleFor(String::toString)
        .must(PredicateBuilder.<String>from(stringEquals("123")).or(stringEquals("456"))).withCode("fail")
        .must(not(stringEquals("321"))).withCode(fn -> { throw new RuntimeException(); } );

    }

  }

  public static class ObjectFrom {

    public String value;

    public List<String> values;

    public String getValue() {
      return value;
    }

    public void setValue(final String value) {
      this.value = value;
    }

    public List<String> getValues() {
      return values;
    }

    public void setValues(final List<String> values) {
      this.values = values;
    }

  }

}
// @formatter:on
