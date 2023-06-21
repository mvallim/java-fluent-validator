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

package br.com.fluentvalidator.validator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalObject;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Gender;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.predicate.PredicateBuilder;

public class ValidatorGirl extends AbstractValidator<Girl> {

    @Override
    public void rules() {

        ruleFor(Girl::getGender).must(PredicateBuilder.from(equalObject(Gender.FEMALE))).when(not(nullValue())).withMessage("gender of girl must be FEMALE").withFieldName("gender");

        ruleFor(Girl::getName).must(PredicateBuilder.from(stringContains("Ana"))).when(not(stringEmptyOrNull())).withMessage("child name must contains key Ana")
                .withFieldName("name");
    }

}
