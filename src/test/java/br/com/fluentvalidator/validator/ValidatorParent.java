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

import static br.com.fluentvalidator.predicate.CollectionPredicate.empty;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.stream.Collectors;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.handler.HandlerInvalidField;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Child;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.model.Parent;
import br.com.fluentvalidator.predicate.PredicateBuilder;

public class ValidatorParent extends AbstractValidator<Parent> {

    @Override
    public void rules() {

        setPropertyOnContext("parent");

        ruleForEach(Parent::getChildren)
          .must(not(nullValue()))
            .handlerInvalidField(new HandlerInvalidField<Collection<Child>>() {
              @Override
              public Collection<Error> handle(final Collection<Child> attemptedValue) {
                return Collections.singletonList(Error.create("children", "parent's children cannot be null", "555", attemptedValue));
              }
            })
          .must(not(empty()))
            .when(not(nullValue()))
            .withMessage("parent must have at least one child")
            .withFieldName("children")
          .whenever(not(nullValue()))
            .withValidator(new ValidatorChild())
            .critical()
          .whenever(not(nullValue()));

        ruleFor(Parent::getId)
        	.whenever(not(nullValue()))
        		.withValidator(new ValidatorId())
        		.critical();

        ruleFor(Parent::getAge)
        	.must(greaterThanOrEqual(5))
        		.when(not(nullValue()))
        		.withMessage("age must be greater than or equal to 10")
        		.withFieldName("age")
            .must(lessThanOrEqual(7))
            	.when(not(nullValue()))
        		.withMessage("age must be less than or equal to 7")
    			.withCode("666")
    			.withFieldName("age");

        ruleFor(Parent::getCities)
        	.must(hasSize(10))
        		.when(not(nullValue()))
    			.withMessage("cities size must be 10")
    			.withFieldName("cities");

        ruleFor(Parent::getName)
        	.must(stringContains("John"))
        		.when(not(stringEmptyOrNull()))
        		.withMessage("name must contains key John")
        		.withFieldName("name");

        ruleForEach(parent -> extractGirls(parent.getChildren()))
        	.whenever(PredicateBuilder.<Collection<Girl>>from(not(nullValue())).and(not(empty())))
                .withValidator(new ValidatorGirl()).critical();

        ruleForEach(parent -> extractBoys(parent.getChildren()))
        	.whenever(PredicateBuilder.<Collection<Boy>>from(not(nullValue())).and(not(empty())))
                .withValidator(new ValidatorBoy()).critical();

    }

    private Collection<Girl> extractGirls(final Collection<Child> children) {
        return Optional.ofNullable(children)
    			.orElseGet(ArrayList::new)
    			.stream()
    			.filter(Girl.class::isInstance)
    			.map(Girl.class::cast)
    			.collect(Collectors.toList());
    }

    private Collection<Boy> extractBoys(final Collection<Child> children) {
        return Optional.ofNullable(children)
    			.orElseGet(ArrayList::new)
    			.stream()
    			.filter(Boy.class::isInstance)
    			.map(Boy.class::cast)
    			.collect(Collectors.toList());
    }

}
