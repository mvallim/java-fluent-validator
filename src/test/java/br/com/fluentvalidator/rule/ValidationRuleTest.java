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

package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Test;

import br.com.fluentvalidator.context.ValidationContext;

public class ValidationRuleTest {

    @After
    public void tearDown() {
        ValidationContext.remove();
    }

    @Test
    public void testSuccessWhen() {

        final StringValidationRule rule = new StringValidationRule();

        assertTrue(rule.getWhen().test(null));
        assertTrue(rule.getWhen().test("o"));
    }

    @Test
    public void testSuccessWhenExplicited() {

        final StringValidationRule rule = new StringValidationRule();
        rule.when(not(nullValue()));

        assertFalse(rule.getWhen().test(null));
        assertTrue(rule.getWhen().test("o"));
    }

    @Test
    public void testSuccessWhenever() {

        final StringValidationRule rule = new StringValidationRule();

        assertTrue(rule.getWhenever().test(null));
        assertTrue(rule.getWhenever().test("o"));
    }

    @Test
    public void testSuccessWheneverExplicited() {

        final StringValidationRule rule = new StringValidationRule();
        rule.whenever(not(nullValue()));

        assertFalse(rule.getWhenever().test(null));
        assertTrue(rule.getWhenever().test("o"));
    }

    @Test
    public void testSuccessMust() {

        final StringValidationRule rule = new StringValidationRule();

        assertTrue(rule.getMust().test(null));
        assertTrue(rule.getMust().test("o"));
    }

    @Test
    public void testSuccessMustExplicited() {

        final StringValidationRule rule = new StringValidationRule();
        rule.must(not(nullValue()));

        assertFalse(rule.getMust().test(null));
        assertTrue(rule.getMust().test("o"));
    }

    @Test
    public void testSuccessApply() {

        final StringValidationRule rule = new StringValidationRule();
        rule.must(not(nullValue()));

        assertTrue(rule.apply(null));
        assertTrue(rule.apply("o"));
    }

    @Test
    public void testSuccessCritical() {

        final StringValidationRule rule = new StringValidationRule();
        rule.must(not(nullValue()));
        rule.critical();

        assertFalse(rule.apply(null));
        assertTrue(rule.apply("o"));
    }

    class StringValidationRule extends AbstractValidationRule<String, String> {

        @Override
        public boolean apply(final String instance) {
            final boolean apply = getMust().test(instance);
            return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
        }

        @Override
        public boolean support(final String instance) {
            return Boolean.TRUE.equals(getWhen().test(instance));
        }

    }

}
