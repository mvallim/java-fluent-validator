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

package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateAfter;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateAfterOrEqual;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateAfterOrEqualToday;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateAfterToday;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateBefore;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateBeforeOrEqual;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateBeforeOrEqualToday;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateBeforeToday;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateBetween;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateBetweenOrEqual;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateEqualTo;
import static br.com.fluentvalidator.predicate.LocalDatePredicate.localDateIsToday;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.junit.Test;

public class LocalDatePredicateTest {

  // region localDateAfterToday

  @Test
  public void testLocalDateAfterToday() {
    assertFalse(localDateAfterToday().test(LocalDate.now()));

    assertFalse(localDateAfterToday().test(LocalDate.now().minusDays(1)));
    assertFalse(localDateAfterToday().test(LocalDate.now().minusMonths(1)));
    assertFalse(localDateAfterToday().test(LocalDate.now().minusYears(1)));

    assertTrue(localDateAfterToday().test(LocalDate.now().plusDays(1)));
    assertTrue(localDateAfterToday().test(LocalDate.now().plusMonths(1)));
    assertTrue(localDateAfterToday().test(LocalDate.now().plusYears(1)));

    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));

    assertTrue(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertTrue(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertTrue(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateAfterToday() {
    assertFalse(localDateAfterToday().test((LocalDate) null));

    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(null));
    assertFalse(localDateAfterToday(null).test(null));
  }

  // endregion

  // region localDateAfterOrEqualToday

  @Test
  public void testLocalDateAfterOrEqualToday() {
    assertTrue(localDateAfterOrEqualToday().test(LocalDate.now()));

    assertFalse(localDateAfterOrEqualToday().test(LocalDate.now().minusDays(1)));
    assertFalse(localDateAfterOrEqualToday().test(LocalDate.now().minusMonths(1)));
    assertFalse(localDateAfterOrEqualToday().test(LocalDate.now().minusYears(1)));

    assertTrue(localDateAfterOrEqualToday().test(LocalDate.now().plusDays(1)));
    assertTrue(localDateAfterOrEqualToday().test(LocalDate.now().plusMonths(1)));
    assertTrue(localDateAfterOrEqualToday().test(LocalDate.now().plusYears(1)));

    assertTrue(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));

    assertTrue(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertTrue(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertTrue(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateAfterOrEqualToday() {
    assertFalse(localDateAfterOrEqualToday().test((LocalDate) null));

    assertFalse(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateAfterOrEqualToday(ObjectFrom<LocalDate>::getSource).test(null));
    assertFalse(localDateAfterOrEqualToday(null).test(null));
  }

  // endregion

  // region localDateBeforeToday

  @Test
  public void testLocalDateBeforeToday() {
    assertFalse(localDateBeforeToday().test(LocalDate.now()));

    assertTrue(localDateBeforeToday().test(LocalDate.now().minusDays(1)));
    assertTrue(localDateBeforeToday().test(LocalDate.now().minusMonths(1)));
    assertTrue(localDateBeforeToday().test(LocalDate.now().minusYears(1)));

    assertFalse(localDateBeforeToday().test(LocalDate.now().plusDays(1)));
    assertFalse(localDateBeforeToday().test(LocalDate.now().plusMonths(1)));
    assertFalse(localDateBeforeToday().test(LocalDate.now().plusYears(1)));

    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertTrue(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertTrue(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertTrue(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));

    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBeforeToday() {
    assertFalse(localDateBeforeToday().test((LocalDate) null));

    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(null));
    assertFalse(localDateBeforeToday(null).test(null));
  }

  // endregion

  // region localDateBeforeOrEqualToday

  @Test
  public void testLocalDateBeforeOrEqualToday() {
    assertTrue(localDateBeforeOrEqualToday().test(LocalDate.now()));

    assertTrue(localDateBeforeOrEqualToday().test(LocalDate.now().minusDays(1)));
    assertTrue(localDateBeforeOrEqualToday().test(LocalDate.now().minusMonths(1)));
    assertTrue(localDateBeforeOrEqualToday().test(LocalDate.now().minusYears(1)));

    assertFalse(localDateBeforeOrEqualToday().test(LocalDate.now().plusDays(1)));
    assertFalse(localDateBeforeOrEqualToday().test(LocalDate.now().plusMonths(1)));
    assertFalse(localDateBeforeOrEqualToday().test(LocalDate.now().plusYears(1)));

    assertTrue(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertTrue(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertTrue(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertTrue(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));

    assertFalse(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateBeforeOrEqualToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBeforeOrEqualToday() {
    assertFalse(localDateBeforeToday().test((LocalDate) null));

    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(null));
    assertFalse(localDateBeforeToday(null).test(null));
  }

  // endregion

  // region localDateIsToday

  @Test
  public void testLocalDateIsToday() {
    assertTrue(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateIsToday() {
    assertFalse(localDateIsToday().test(null));

    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateIsToday(ObjectFrom<LocalDate>::getSource).test(null));
    assertFalse(localDateIsToday(null).test(null));
  }

  // endregion

  // region localDateEqualTo

  @Test
  public void testLocalDateEqualTo() {
    assertTrue(localDateEqualTo(LocalDate.now()).test(LocalDate.now()));
    assertFalse(localDateEqualTo(LocalDate.now()).test(LocalDate.now().plusDays(1)));
    assertFalse(localDateEqualTo(LocalDate.now()).test(LocalDate.now().minusDays(1)));

    assertTrue(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now().plusDays(1)).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));
    assertTrue(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now().plusDays(1)).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));

    assertFalse(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1)).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));
    assertTrue(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1)).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateEqualTo() {
    assertFalse(localDateEqualTo(LocalDate.now()).test(null));
    assertFalse(localDateEqualTo(null).test(LocalDate.now()));
    assertFalse(localDateEqualTo(null).test(null));

    assertFalse(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(null));
    assertFalse(localDateEqualTo(ObjectFrom<LocalDate>::getSource, null).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));
    assertFalse(localDateEqualTo(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateEqualTo(null, LocalDate.now()).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateEqualTo(null, LocalDate.now()).test(null));
  }

  // endregion

  // region localDateAfter

  @Test
  public void testLocalDateAfter() {
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertTrue(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertTrue(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertTrue(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateAfter() {
    assertFalse(localDateAfter(LocalDate.now()).test(null));
    assertFalse(localDateAfter(null).test(LocalDate.now()));
    assertFalse(localDateAfter(null).test(null));

    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(null));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateAfter(null, (LocalDate) null).test(null));

    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(null));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateAfter(null, (LocalDate) null).test(null));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), null)));
  }

  // endregion

  // region localDateAfterOrEqual

  @Test
  public void testLocalDateAfterOrEqual() {
    assertTrue(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertTrue(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertTrue(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertTrue(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateAfterOrEqual() {
    assertFalse(localDateAfterOrEqual(LocalDate.now()).test(null));
    assertFalse(localDateAfterOrEqual(null).test(LocalDate.now()));
    assertFalse(localDateAfterOrEqual(null).test(null));

    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(null));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateAfterOrEqual(null, (LocalDate) null).test(null));

    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(null));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateAfterOrEqual(null, (LocalDate) null).test(null));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateAfterOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), null)));
  }

  // endregion

  // region localDateBefore

  @Test
  public void testLocalDateBefore() {
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertTrue(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertTrue(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertTrue(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBefore() {
    assertFalse(localDateBefore(LocalDate.now()).test(null));
    assertFalse(localDateBefore(null).test(LocalDate.now()));
    assertFalse(localDateBefore(null).test(null));

    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(null));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateBefore(null, (LocalDate) null).test(null));

    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(null));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateBefore(null, (LocalDate) null).test(null));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), null)));
  }

  // endregion

  // region localDateBeforeOrEqual

  @Test
  public void testLocalDateBeforeOrEqual() {
    assertTrue(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertTrue(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertTrue(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertTrue(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBeforeOrEqual() {
    assertFalse(localDateBeforeOrEqual(LocalDate.now()).test(null));
    assertFalse(localDateBeforeOrEqual(null).test(LocalDate.now()));
    assertFalse(localDateBeforeOrEqual(null).test(null));

    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, LocalDate.now()).test(null));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateBeforeOrEqual(null, (LocalDate) null).test(null));

    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(null));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, (LocalDate) null).test(null));
    assertFalse(localDateBeforeOrEqual(null, (LocalDate) null).test(null));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(null, LocalDate.now())));
    assertFalse(localDateBeforeOrEqual(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<>(LocalDate.now(), null)));
  }

  // endregion

  // region localDateBetween

  @Test
  public void testLocalDateBetweenLocalDate() {
    assertTrue(localDateBetween(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(LocalDate.now()));
    assertFalse(localDateBetween(LocalDate.now(), LocalDate.now().plusDays(1)).test(LocalDate.now()));
    assertFalse(localDateBetween(LocalDate.now().minusDays(1), LocalDate.now()).test(LocalDate.now()));
    assertFalse(localDateBetween(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(LocalDate.now().plusDays(5)));
    assertFalse(localDateBetween(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(LocalDate.now().minusDays(5)));

    assertTrue(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(
        localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now().plusDays(5), LocalDate.now().minusDays(1), LocalDate.now().plusDays(1))));
    assertFalse(
        localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now().minusDays(5), LocalDate.now().minusDays(1), LocalDate.now().plusDays(1))));

    assertTrue(localDateBetween(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, LocalDate.now(), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now())));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now().plusDays(5), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now().minusDays(5), null, LocalDate.now().plusDays(1))));

    assertTrue(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now()).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now().plusDays(5), LocalDate.now().minusDays(1), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now().minusDays(5), LocalDate.now().minusDays(1), null)));
  }

  @Test
  public void testNullObjectLocalDateBetweenLocalDate() {
    assertFalse(localDateBetween(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(null));
    assertFalse(localDateBetween(null, LocalDate.now().plusDays(1)).test(LocalDate.now()));
    assertFalse(localDateBetween(LocalDate.now().minusDays(1), null).test(LocalDate.now()));
    assertFalse(localDateBetween(null, null).test(LocalDate.now()));
    assertFalse(localDateBetween(null, null).test(null));

    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, null, null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(null));
    assertFalse(localDateBetween(null, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (Function<ObjectFromLocalDate, LocalDate>) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(null, (Function<ObjectFromLocalDate, LocalDate>) null, (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));

    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, null, null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(null));
    assertFalse(localDateBetween(null, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(null, (LocalDate) null, (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));

    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(null, LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), null, null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(null, null, null)));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(null));
    assertFalse(localDateBetween(null, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, (Function<ObjectFromLocalDate, LocalDate>) null, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetween(null, (Function<ObjectFromLocalDate, LocalDate>) null, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
  }

  // endregion

  // region localDateBetweenOrEqual

  @Test
  public void testLocalDateBetweenOrEqual() {
    assertTrue(localDateBetweenOrEqual(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(LocalDate.now()));
    assertTrue(localDateBetweenOrEqual(LocalDate.now(), LocalDate.now().plusDays(1)).test(LocalDate.now()));
    assertTrue(localDateBetweenOrEqual(LocalDate.now().minusDays(1), LocalDate.now()).test(LocalDate.now()));
    assertFalse(localDateBetweenOrEqual(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(LocalDate.now().plusDays(5)));
    assertFalse(localDateBetweenOrEqual(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(LocalDate.now().minusDays(5)));

    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), LocalDate.now().plusDays(1))));
    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax)
        .test(new ObjectFromLocalDate(LocalDate.now().plusDays(5), LocalDate.now().minusDays(1), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax)
        .test(new ObjectFromLocalDate(LocalDate.now().minusDays(5), LocalDate.now().minusDays(1), LocalDate.now().plusDays(1))));

    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, LocalDate.now(), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now())));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now().plusDays(5), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now().minusDays(5), null, LocalDate.now().plusDays(1))));

    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), null)));
    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertTrue(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now()).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now().minusDays(1), null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now().plusDays(5), LocalDate.now().minusDays(1), null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now().minusDays(5), LocalDate.now().minusDays(1), null)));
  }

  @Test
  public void testNullObjectLocalDateBetweenOrEqual() {
    assertFalse(localDateBetweenOrEqual(LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(null));
    assertFalse(localDateBetweenOrEqual(null, LocalDate.now().plusDays(1)).test(LocalDate.now()));
    assertFalse(localDateBetweenOrEqual(LocalDate.now().minusDays(1), null).test(LocalDate.now()));
    assertFalse(localDateBetweenOrEqual(null, null).test(LocalDate.now()));
    assertFalse(localDateBetweenOrEqual(null, null).test(null));

    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, null, null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(null));
    assertFalse(localDateBetweenOrEqual(null, ObjectFromLocalDate::getMin, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(
        localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (Function<ObjectFromLocalDate, LocalDate>) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(
        localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(null, (Function<ObjectFromLocalDate, LocalDate>) null, (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));

    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), null, null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(null, null, null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(null));
    assertFalse(localDateBetweenOrEqual(null, LocalDate.now().minusDays(1), ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (LocalDate) null, ObjectFromLocalDate::getMax).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(
        localDateBetweenOrEqual(ObjectFromLocalDate::getSource, LocalDate.now().minusDays(1), (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(null, (LocalDate) null, (Function<ObjectFromLocalDate, LocalDate>) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));

    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(null, LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), null, LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), null, null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(null, null, null)));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(null));
    assertFalse(localDateBetweenOrEqual(null, ObjectFromLocalDate::getMin, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(
        localDateBetweenOrEqual(ObjectFromLocalDate::getSource, (Function<ObjectFromLocalDate, LocalDate>) null, LocalDate.now().plusDays(1)).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(ObjectFromLocalDate::getSource, ObjectFromLocalDate::getMin, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
    assertFalse(localDateBetweenOrEqual(null, (Function<ObjectFromLocalDate, LocalDate>) null, (LocalDate) null).test(new ObjectFromLocalDate(LocalDate.now(), LocalDate.now(), LocalDate.now().plusDays(1))));
  }

  // endregion

  // region multi thread test

  @Test
  public void testLocalDatePredicateMultiThreadMustBeTrue() throws InterruptedException {
    final int CONCURRENT_RUNNABLE = 100_000;
    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();
    final ExecutorService executorService = Executors.newFixedThreadPool(10);

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          assertThatCode(() -> {
            resultsOne.add(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(new ObjectFrom<>(LocalDate.now(), LocalDate.now())));
          }).doesNotThrowAnyException();
        }
      });
    }

    executorService.shutdown();

    executorService.awaitTermination(10, TimeUnit.MINUTES);

    assertThat(resultsOne, hasSize(CONCURRENT_RUNNABLE));

    for (final Boolean result : resultsOne) {
      assertTrue(result);
    }
  }

  // endregion

  private class ObjectFromLocalDate extends ObjectFrom<LocalDate> {

    private final LocalDate min;

    private final LocalDate max;

    public ObjectFromLocalDate(final LocalDate source, final LocalDate min, final LocalDate max) {
      super(source, null);
      this.min = min;
      this.max = max;
    }

    public LocalDate getMin() {
      return min;
    }

    public LocalDate getMax() {
      return max;
    }

  }

}
