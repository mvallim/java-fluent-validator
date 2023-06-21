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

package br.com.fluentvalidator.spring.validator;

import static br.com.fluentvalidator.predicate.StringPredicate.stringMatches;

import org.springframework.stereotype.Component;

import br.com.fluentvalidator.AbstractValidator;

@Component
public class ValidatorSpringId extends AbstractValidator<String> {

    private static final String UUID_REGEX = "[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}";

    @Override
    public void rules() {

        setPropertyOnContext("id");

        ruleFor(id -> id).must(stringMatches(UUID_REGEX)).withMessage("id not matching the pattern of a UUID").withFieldName("id").critical();
    }

}
