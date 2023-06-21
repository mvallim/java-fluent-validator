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

import static br.com.fluentvalidator.predicate.TimePredicate.timeBetween;
import static br.com.fluentvalidator.predicate.TimePredicate.timeEqualTo;
import static br.com.fluentvalidator.predicate.TimePredicate.timeGreaterThan;
import static br.com.fluentvalidator.predicate.TimePredicate.timeGreaterThanOrEqual;
import static br.com.fluentvalidator.predicate.TimePredicate.timeLessThan;
import static br.com.fluentvalidator.predicate.TimePredicate.timeLessThanOrEqual;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

public class TimePredicateTest {

  private static final String HH_MM_SS = "HH:mm:ss";

  //// timeEqualTo

  @Test
  public void testNullTimeEqualTo() {
    assertFalse(timeEqualTo(null, null).test(null));
    assertFalse(timeEqualTo(null, null).test("03:59:59"));
    assertFalse(timeEqualTo(null, HH_MM_SS).test("03:59:59"));
    assertFalse(timeEqualTo(null, HH_MM_SS).test(null));
    assertFalse(timeEqualTo("03:59:59", HH_MM_SS).test(null));
    assertFalse(timeEqualTo("03:59:59", null).test(null));
    assertFalse(timeEqualTo("03:59:59", null).test("03:59:59"));
  }

  @Test
  public void testTimeEqualToInvalid() {
    assertFalse(timeEqualTo("03:59:59", HH_MM_SS).test("00/00:00"));
    assertFalse(timeEqualTo("03:59:59", "HH/mm:ss").test("03:59:59"));
    assertFalse(timeEqualTo("00/00:00", HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testTimeEqualTo() {
    assertFalse(timeEqualTo("03:03:03", HH_MM_SS).test("03:03:02"));
    assertTrue(timeEqualTo("03:03:03", HH_MM_SS).test("03:03:03"));
    assertFalse(timeEqualTo("03:03:03", HH_MM_SS).test("03:03:04"));
  }

  //// timeGreaterThan

  @Test
  public void testNullTimeGreaterThan() {
    assertFalse(timeGreaterThan(null, null).test(null));
    assertFalse(timeGreaterThan(null, null).test("03:59:59"));
    assertFalse(timeGreaterThan(null, HH_MM_SS).test("03:59:59"));
    assertFalse(timeGreaterThan(null, HH_MM_SS).test(null));
    assertFalse(timeGreaterThan("03:59:59", HH_MM_SS).test(null));
    assertFalse(timeGreaterThan("03:59:59", null).test(null));
    assertFalse(timeGreaterThan("03:59:59", null).test("03:59:59"));
  }

  @Test
  public void testTimeGreaterThanInvalid() {
    assertFalse(timeGreaterThan("03:59:59", HH_MM_SS).test("03/59:59"));
    assertFalse(timeGreaterThan("03:59:59", "HH/mm:ss").test("03:59:59"));
    assertFalse(timeGreaterThan("03/59:59", HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testTimeGreaterThan() {
    assertTrue(timeGreaterThan("03:03:03", HH_MM_SS).test("03:03:04"));
    assertFalse(timeGreaterThan("03:03:04", HH_MM_SS).test("03:03:04"));
    assertFalse(timeGreaterThan("03:03:05", HH_MM_SS).test("03:03:04"));
  }

  //// timeLessThan

  @Test
  public void testNullTimeLessThanThan() {
    assertFalse(timeLessThan(null, null).test(null));
    assertFalse(timeLessThan(null, null).test(""));
    assertFalse(timeLessThan(null, "HH:mm-ss").test(""));
    assertFalse(timeLessThan(null, HH_MM_SS).test(null));
    assertFalse(timeLessThan("", HH_MM_SS).test(null));
    assertFalse(timeLessThan("", null).test(null));
    assertFalse(timeLessThan("", null).test(""));
  }

  @Test
  public void testTimeLessThanInvalid() {
    assertFalse(timeLessThan("03:59:59", HH_MM_SS).test("03/59:59"));
    assertFalse(timeLessThan("03:59:59", "HH/mm:ss").test("03:59:59"));
    assertFalse(timeLessThan("03/59:59", HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testTimeLessThan() {
    assertTrue(timeLessThan("03:03:05", HH_MM_SS).test("03:03:04"));
    assertFalse(timeLessThan("03:03:04", HH_MM_SS).test("03:03:04"));
    assertFalse(timeLessThan("03:03:03", HH_MM_SS).test("03:03:04"));
  }

  //// timeGreaterThanOrEqual

  @Test
  public void testNullTimeGreaterThanOrEqual() {
    assertFalse(timeGreaterThanOrEqual(null, null).test(null));
    assertFalse(timeGreaterThanOrEqual(null, null).test("03:59:59"));
    assertFalse(timeGreaterThanOrEqual(null, HH_MM_SS).test("03:59:59"));
    assertFalse(timeGreaterThanOrEqual(null, HH_MM_SS).test(null));
    assertFalse(timeGreaterThanOrEqual("03:59:59", HH_MM_SS).test(null));
    assertFalse(timeGreaterThanOrEqual("03:59:59", null).test(null));
    assertFalse(timeGreaterThanOrEqual("03:59:59", null).test("03:59:59"));
  }

  @Test
  public void testTimeGreaterThanOrEqualInvalid() {
    assertFalse(timeGreaterThanOrEqual("03:59:59", HH_MM_SS).test("03/59:59"));
    assertFalse(timeGreaterThanOrEqual("03:59:59", "HH/mm:ss").test("03:59:59"));
    assertFalse(timeGreaterThanOrEqual("03/59:59", HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testTimeGreaterThanOrEqual() {
    assertTrue(timeGreaterThanOrEqual("03:59:58", HH_MM_SS).test("03:59:59"));
    assertTrue(timeGreaterThanOrEqual("03:59:59", HH_MM_SS).test("03:59:59"));
    assertFalse(timeGreaterThanOrEqual("04:00:00", HH_MM_SS).test("03:59:59"));
  }

  //// timeLessThanOrEqual

  @Test
  public void testNullDateTimeLessThanOrEqual() {
    assertFalse(timeLessThanOrEqual(null, null).test(null));
    assertFalse(timeLessThanOrEqual(null, null).test("03:59:59"));
    assertFalse(timeLessThanOrEqual(null, HH_MM_SS).test("03:59:59"));
    assertFalse(timeLessThanOrEqual(null, HH_MM_SS).test(null));
    assertFalse(timeLessThanOrEqual("03:59:59", HH_MM_SS).test(null));
    assertFalse(timeLessThanOrEqual("03:59:59", null).test(null));
    assertFalse(timeLessThanOrEqual("03:59:59", null).test("03:59:59"));
  }

  @Test
  public void testDateLessTimeThanOrEqualInvalid() {
    assertFalse(timeLessThanOrEqual("03:59:59", HH_MM_SS).test("03/59:59"));
    assertFalse(timeLessThanOrEqual("03:59:59", "yyyy/MM-dd").test("03:59:59"));
    assertFalse(timeLessThanOrEqual("03/59:59", HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testDateLessTimeThanOrEqual() {
    assertTrue(timeLessThanOrEqual("04:00:00", HH_MM_SS).test("03:59:59"));
    assertTrue(timeLessThanOrEqual("03:59:59", HH_MM_SS).test("03:59:59"));
    assertFalse(timeLessThanOrEqual("03:59:58", HH_MM_SS).test("03:59:59"));
  }
  //// timeBetween(String, String, pattern)

  @Test
  public void testNullTimeBetween() {
    assertFalse(timeBetween("03:59:59", null, null).test(null));
    assertFalse(timeBetween(null, "03:59:59", null).test(null));
    assertFalse(timeBetween(null, null, HH_MM_SS).test(null));
    assertFalse(timeBetween(null, null, null).test("03:59:59"));
    assertFalse(timeBetween("03:59:59", "03:59:59", null).test(null));
    assertFalse(timeBetween(null, "03:59:59", HH_MM_SS).test(null));
    assertFalse(timeBetween(null, null, HH_MM_SS).test(""));
    assertFalse(timeBetween("03:59:59", "03:59:59", HH_MM_SS).test(null));
    assertFalse(timeBetween(null, "03:59:59", HH_MM_SS).test("03:59:59"));
    assertFalse(timeBetween(null, null, HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testTimeBetweenInvalid() {
    assertFalse(timeBetween("03:59:59", "03:59:59", HH_MM_SS).test("03/59:59"));
    assertFalse(timeBetween("03:59:59", "03:59:59", "HH:mm/ss").test("03:59:59"));
    assertFalse(timeBetween("03:59:59", "03:59/59", HH_MM_SS).test("03:59:59"));
    assertFalse(timeBetween("03:59/59", "03:59:59", HH_MM_SS).test("03:59:59"));
  }

  @Test
  public void testTimeBetween() {
    assertFalse(timeBetween("03:59:57", "03:59:59", HH_MM_SS).test("03:59:56"));
    assertTrue(timeBetween("03:59:57", "03:59:59", HH_MM_SS).test("03:59:57"));
    assertTrue(timeBetween("03:59:57", "03:59:59", HH_MM_SS).test("03:59:58"));
    assertTrue(timeBetween("03:59:57", "03:59:59", HH_MM_SS).test("03:59:59"));
    assertFalse(timeBetween("03:59:57", "03:59:59", HH_MM_SS).test("04:00:00"));
  }

  //// timeGreaterThan(Function, Function, pattern)

  @Test
  public void testObjectTimeGreaterThan() {
    assertTrue(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:58")));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "04:00:00")));
  }

  @Test
  public void testNullObjectTimeGreaterThan() {
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(null));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeGreaterThan(Function, String, pattern)

  @Test
  public void testObjectTimeGreaterThan2() {
    assertTrue(timeGreaterThan(ObjectFrom<String>::getSource, "03:59:58", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, "04:00:00", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
  }

  @Test
  public void testNullObjectTimeGreaterThan2() {
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(null));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeGreaterThan(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeGreaterThanOrEqual(Function, Function, pattern)

  @Test
  public void testObjectTimeGreaterThanOrEqual() {
    assertTrue(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:58")));
    assertTrue(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "04:00:00")));
  }

  @Test
  public void testNullObjectTimeGreaterThanOrEqual() {
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(null));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeGreaterThanOrEqual(Function, String, pattern)

  @Test
  public void testObjectTimeGreaterThanOrEqual2() {
    assertTrue(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, "03:59:58", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertTrue(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, "04:00:00", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
  }

  @Test
  public void testNullObjectTimeGreaterThanOrEqual2() {
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(null));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeGreaterThanOrEqual(ObjectFrom<String>::getSource, (String) null, " HH:mm:ss").test(new ObjectFrom<String>(null, null)));
  }

  //// timeLessThan(Function, Function, pattern)

  @Test
  public void testObjectTimeLessThan() {
    assertTrue(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "04:00:00")));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:58")));
  }

  @Test
  public void testNullObjectTimeLessThan() {
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(null));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeLessThan(Function, String, pattern)

  @Test
  public void testObjectTimeLessThan2() {
    assertTrue(timeLessThan(ObjectFrom<String>::getSource, "04:00:00", HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, "03:59:58", HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
  }

  @Test
  public void testNullObjectTimeLessThan2() {
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(null));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeLessThan(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeLessThanOrEqual(Function, Function, pattern)

  @Test
  public void testObjectTimeLessThanOrEqual() {
    assertTrue(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "04:00:00")));
    assertTrue(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:58")));
  }

  @Test
  public void testNullObjectTimeLessThanOrEqual() {
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(null));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeLessThanOrEqual(Function, String, pattern)

  @Test
  public void testObjectTimeLessThanOrEqual2() {
    assertTrue(timeLessThanOrEqual(ObjectFrom<String>::getSource, "04:00:00", HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertTrue(timeLessThanOrEqual(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, "03:59:58", HH_MM_SS).test(new ObjectFrom<>("03:59:59", "03:59:59")));
  }

  @Test
  public void testNullObjectTimeLessThanOrEqual2() {
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(null));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, "03:59:59", HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeLessThanOrEqual(ObjectFrom<String>::getSource, (String) null, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
  }

  //// timeBetween(Function, String, String, pattern)

  @Test
  public void testNullObjectTimeBetween() {
    assertFalse(timeBetween(ObjectFrom<String>::getSource, "03:59:59", (String) null, (String) null).test(null));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, "03:59:59", (String) null).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, (String) null, HH_MM_SS).test(new ObjectFrom<String>(null, null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, (String) null, (String) null).test(new ObjectFrom<String>(null, null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, "03:59:59", "03:59:59", null).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, (String) null, "yyyy-MM-dd").test(new ObjectFrom<>(null, "")));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, "03:59:59", "03:59:59", "yyyy-MM-dd").test(new ObjectFrom<String>(null, null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, "03:59:59", HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, null, (String) null, HH_MM_SS).test(new ObjectFrom<>(null, "03:59:59")));
  }

  @Test
  public void testObjectTimeBetween() {
    assertFalse(timeBetween(ObjectFrom<String>::getSource, "03:59:57", "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:56", null)));
    assertTrue(timeBetween(ObjectFrom<String>::getSource, "03:59:57", "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:57", null)));
    assertTrue(timeBetween(ObjectFrom<String>::getSource, "03:59:57", "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:58", null)));
    assertTrue(timeBetween(ObjectFrom<String>::getSource, "03:59:57", "03:59:59", HH_MM_SS).test(new ObjectFrom<>("03:59:59", null)));
    assertFalse(timeBetween(ObjectFrom<String>::getSource, "03:59:57", "03:59:59", HH_MM_SS).test(new ObjectFrom<>("04:00:00", null)));
  }

  //// multi thread test

  @Test
  public void testTimePredicateMultiThreadMustBeTrue() throws InterruptedException {

    final int CONCURRENT_RUNNABLE = 100000;

    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();

    final ExecutorService executorService = Executors.newFixedThreadPool(10);

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          assertThatCode(() -> {
            resultsOne.add(timeBetween("2018-06-22T10:00:00", "2018-06-22T10:00:00", "yyyy-MM-dd'T'HH:mm:ss").test("2018-06-22T10:00:00"));
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

}
