package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeBetween;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeEqualTo;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeGreaterThan;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeGreaterThanOrEqual;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeLessThan;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeLessThanOrEqual;
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

public class DateTimePredicateTest {

  private static final String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss";

  //// dateTimeEqualTo

  @Test
  public void testNullDateTimeEqualTo() {
    assertFalse(dateTimeEqualTo(null, null).test(null));
    assertFalse(dateTimeEqualTo(null, null).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeEqualTo(null, DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeEqualTo(null, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeEqualTo("2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeEqualTo("2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeEqualTo("2019-09-19 00:00:00", null).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeEqualToInvalid() {
    assertFalse(dateTimeEqualTo("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-40 00:00:00"));
    assertFalse(dateTimeEqualTo("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019/09-19 00:00:00"));
    assertFalse(dateTimeEqualTo("2019-09-19 00:00:00", "yyyy/MM-dd HH:mm:ss").test("2019-09-19 00:00:00"));
    assertFalse(dateTimeEqualTo("2019/09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeEqualToDateTime() {
    assertTrue(dateTimeEqualTo("2019-09-19 03:03:03", DATE_TIME_FORMAT).test("2019-09-19 03:03:03"));
    assertFalse(dateTimeEqualTo("2019-09-19 03:03:03", DATE_TIME_FORMAT).test("2019-09-19 03:03:04"));
  }

  //// dateTimeGreaterThan

  @Test
  public void testNullDateTimeGreaterThan() {
    assertFalse(dateTimeGreaterThan(null, null).test(null));
    assertFalse(dateTimeGreaterThan(null, null).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThan(null, DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThan(null, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThan("2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThan("2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeGreaterThan("2019-09-19 00:00:00", null).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeGreaterThanInvalid() {
    assertFalse(dateTimeGreaterThan("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-40 00:00:00"));
    assertFalse(dateTimeGreaterThan("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019/09-19 00:00:00"));
    assertFalse(dateTimeGreaterThan("2019-09-19 00:00:00", "yyyy/MM-dd HH:mm:ss").test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThan("2019/09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeGreaterThanDateTime() {
    assertTrue(dateTimeGreaterThan("2019-09-19 03:03:03", DATE_TIME_FORMAT).test("2019-09-19 03:03:04"));
    assertFalse(dateTimeGreaterThan("2019-09-19 03:03:04", DATE_TIME_FORMAT).test("2019-09-19 03:03:04"));
    assertFalse(dateTimeGreaterThan("2019-09-19 03:03:05", DATE_TIME_FORMAT).test("2019-09-19 03:03:04"));
  }

  //// dateTimeLessThan

  @Test
  public void testNullDateTimeLessThanThan() {
    assertFalse(dateTimeLessThan(null, null).test(null));
    assertFalse(dateTimeLessThan(null, null).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThan(null, DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThan(null, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThan("2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThan("2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeLessThan("2019-09-19 00:00:00", null).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeLessThanInvalid() {
    assertFalse(dateTimeLessThan("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019/09-19 00:00:00"));
    assertFalse(dateTimeLessThan("2019-09-19 00:00:00", "yyyy/MM-dd HH:mm:ss").test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThan("2019/09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeLessThanDate() {
    assertTrue(dateTimeLessThan("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-18 00:00:00"));
    assertFalse(dateTimeLessThan("2019-09-18 00:00:00", DATE_TIME_FORMAT).test("2019-09-18 00:00:00"));
    assertFalse(dateTimeLessThan("2019-09-17 00:00:00", DATE_TIME_FORMAT).test("2019-09-18 00:00:00"));
  }

  //// dateTimeGreaterThanOrEqual

  @Test
  public void testNullDateTimeGreaterThanOrEqual() {
    assertFalse(dateTimeGreaterThanOrEqual(null, null).test(null));
    assertFalse(dateTimeGreaterThanOrEqual(null, null).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThanOrEqual(null, DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThanOrEqual(null, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", null).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeGreaterThanOrEqualInvalid() {
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-40 00:00:00"));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019/09-19 00:00:00"));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", "yyyy/MM-dd HH:mm:ss").test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThanOrEqual("2019/09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeGreaterThanOrEqual() {
    assertTrue(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-20 00:00:00"));
    assertTrue(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-18 00:00:00"));
  }

  //// dateTimeLessThanOrEqual

  @Test
  public void testNullDateTimeLessThanOrEqual() {
    assertFalse(dateTimeLessThanOrEqual(null, null).test(null));
    assertFalse(dateTimeLessThanOrEqual(null, null).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThanOrEqual(null, DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThanOrEqual(null, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", null).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateLessTimeThanOrEqualInvalid() {
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-40 00:00:00"));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019/09-19 00:00:00"));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", "yyyy/MM-dd HH:mm:ss").test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThanOrEqual("2019/09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateLessTimeThanOrEqual() {
    assertTrue(dateTimeLessThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-18 00:00:00"));
    assertTrue(dateTimeLessThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-20 00:00:00"));
  }

  //// dateTimeBetween

  @Test
  public void testNullDateTimeBetween() {
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", null, null).test(null));
    assertFalse(dateTimeBetween(null, "2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeBetween(null, null, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeBetween(null, null, null).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", "2019-09-19 00:00:00", null).test(null));
    assertFalse(dateTimeBetween(null, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeBetween(null, null, DATE_TIME_FORMAT).test("2019-09-19"));
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeBetween(null, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeBetween(null, null, DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeBetweenInvalid() {
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", "2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00/00"));
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", "2019-09-19 00:00:00", "HH:mm/ss").test("2019/09-19 00:00:00"));
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", "2019-09-19 00:00/00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeBetween("2019-09-19 00:00:00", "2019-09/19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeBetween("2019-09-19 00:00/00", "2019-09:19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeBetween("2019-09/19 00:00:00", "2019-09:19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
  }

  @Test
  public void testDateTimeBetween() {
    assertTrue(dateTimeBetween("2019-09-19 00:00:00", "2019-09-19 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertTrue(dateTimeBetween("2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test("2019-09-20 00:00:00"));
    assertTrue(dateTimeBetween("2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test("2019-09-18 00:00:00"));
    assertTrue(dateTimeBetween("2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test("2019-09-19 00:00:00"));
    assertFalse(dateTimeBetween("2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test("2019-09-17 00:00:00"));
  }

  //// dateTimeGreaterThan(Function, Function, pattern)

  @Test
  public void testObjectDateTimeGreaterThan() {
    assertTrue(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-17 00:00:00", "2019-09-18 00:00:00")));
  }

  @Test
  public void testNullObjectDateTimeGreaterThan() {
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeGreaterThan(Function, String, pattern)

  @Test
  public void testObjectDateTimeGreaterThan2() {
    assertTrue(dateTimeGreaterThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-17 00:00:00", null)));
  }

  @Test
  public void testNullObjectDateTimeGreaterThan2() {
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThan(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeGreaterThanOrEqual(Function, Function, pattern)

  @Test
  public void testObjectDateTimeGreaterThanOrEqual() {
    assertTrue(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", "2019-09-18 00:00:00")));
    assertTrue(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-17 00:00:00", "2019-09-18 00:00:00")));
  }

  @Test
  public void testNullObjectDateTimeGreaterThanOrEqual() {
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeGreaterThanOrEqual(Function, String, pattern)

  @Test
  public void testObjectDateTimeGreaterThanOrEqual2() {
    assertTrue(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertTrue(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-17 00:00:00", null)));
  }

  @Test
  public void testNullObjectDateTimeGreaterThanOrEqual2() {
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeGreaterThanOrEqual(ObjectFrom<String>::getSource, (String) null, "yyyy-MM-dd  HH:mm:ss").test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeLessThan(Function, Function, pattern)

  @Test
  public void testObjectDateTimeLessThan() {
    assertTrue(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", "2019-09-19 00:00:00")));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", "2019-09-19 00:00:00")));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-20 00:00:00", "2019-09-19 00:00:00")));
  }

  @Test
  public void testNullObjectDateTimeLessThan() {
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeLessThan(Function, String, pattern)

  @Test
  public void testObjectDateTimeLessThan2() {
    assertTrue(dateTimeLessThan(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", "2019-09-19 00:00:00")));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", "2019-09-19 00:00:00")));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-20 00:00:00", "2019-09-19 00:00:00")));
  }

  @Test
  public void testNullObjectDateTimeLessThan2() {
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeLessThan(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeLessThanOrEqual(Function, Function, pattern)

  @Test
  public void testObjectDateTimeLessThanOrEqual() {
    assertTrue(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", "2019-09-19 00:00:00")));
    assertTrue(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", "2019-09-19 00:00:00")));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-20 00:00:00", "2019-09-19 00:00:00")));
  }

  @Test
  public void testNullObjectDateTimeLessThanOrEqual() {
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeLessThanOrEqual(Function, String, pattern)

  @Test
  public void testObjectDateTimeLessThanOrEqual2() {
    assertTrue(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", "2019-09-19 00:00:00")));
    assertTrue(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", "2019-09-19 00:00:00")));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-20 00:00:00", "2019-09-19 00:00:00")));
  }

  @Test
  public void testNullObjectDateTimeLessThanOrEqual2() {
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(null));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-18 00:00:00")));
    assertFalse(dateTimeLessThanOrEqual(ObjectFrom<String>::getSource, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
  }

  //// dateTimeBetween(Function, String, String, pattern)

  @Test
  public void testNullObjectDateTimeBetween() {
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", (String) null, (String) null).test(null));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, "2019-09-19 00:00:00", (String) null).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<String>(null, null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, (String) null, (String) null).test(new ObjectFrom<String>(null, null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", "2019-09-19 00:00:00", null).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, (String) null, "yyyy-MM-dd").test(new ObjectFrom<>(null, "2019-09-19")));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", "2019-09-19 00:00:00", "yyyy-MM-dd").test(new ObjectFrom<String>(null, null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-19 00:00:00")));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, null, (String) null, DATE_TIME_FORMAT).test(new ObjectFrom<>(null, "2019-09-19 00:00:00")));
  }

  @Test
  public void testObjectDateTimeBetween() {
    assertTrue(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-19 00:00:00", "2019-09-19 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertTrue(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-20 00:00:00", null)));
    assertTrue(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-19 00:00:00", null)));
    assertTrue(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-18 00:00:00", null)));
    assertFalse(dateTimeBetween(ObjectFrom<String>::getSource, "2019-09-18 00:00:00", "2019-09-20 00:00:00", DATE_TIME_FORMAT).test(new ObjectFrom<>("2019-09-17 00:00:00", null)));
  }

  //// multi thread test

  @Test
  public void testDateTimePredicateMultiThreadMustBeTrue() throws InterruptedException {

    final int CONCURRENT_RUNNABLE = 100000;

    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();

    final ExecutorService executorService = Executors.newFixedThreadPool(10);

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          assertThatCode(() -> {
            resultsOne.add(dateTimeBetween("2018-06-22T10:00:00", "2018-06-22T10:00:00", "yyyy-MM-dd'T'HH:mm:ss").test("2018-06-22T10:00:00"));
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
