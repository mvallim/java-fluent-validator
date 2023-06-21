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

package br.com.fluentvalidator.spring.validator;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import org.springframework.stereotype.Component;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Child;
import br.com.fluentvalidator.model.Parent;

@Component
public class ValidatorSpringChild extends AbstractValidator<Child> {

    @Override
    public void rules() {

        setPropertyOnContext("child");

        ruleFor(Child::getAge).must(not(nullValue())).withMessage("child age must be not null").withFieldName("age").critical().must(greaterThanOrEqual(5)).when(not(nullValue()))
                .withMessage("child age must be greater than or equal to 5").withFieldName("age").must(this::checkAgeConstraintChild).when(not(nullValue()))
                .withMessage("child age must be less than age parent").withFieldName("age").critical();

        ruleFor(Child::getName).must(not(stringEmptyOrNull())).withMessage("child name must be not null or empty").withFieldName("name");

    }

    private boolean checkAgeConstraintChild(final Integer age) {
        return age < getPropertyOnContext("parent", Parent.class).getAge();
    }

}
