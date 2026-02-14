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

import static br.com.fluentvalidator.predicate.StringPredicate.isAlpha;
import static br.com.fluentvalidator.predicate.StringPredicate.isAlphaNumeric;
import static br.com.fluentvalidator.predicate.StringPredicate.isDate;
import static br.com.fluentvalidator.predicate.StringPredicate.isDateTime;
import static br.com.fluentvalidator.predicate.StringPredicate.isNumber;
import static br.com.fluentvalidator.predicate.StringPredicate.isNumeric;
import static br.com.fluentvalidator.predicate.StringPredicate.isTime;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEquals;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEqualsIgnoreCase;
import static br.com.fluentvalidator.predicate.StringPredicate.stringInCollection;
import static br.com.fluentvalidator.predicate.StringPredicate.stringMatches;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSize;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeBetween;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeGreaterThan;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeGreaterThanOrEqual;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThan;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThanOrEqual;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

class StringPredicateTest {

  @Test
  void testEquals() {
    assertTrue(stringEquals("xo").test("xo"));
    assertFalse(stringEquals("xo").test("Xo"));
    assertFalse(stringEquals("xo").test(null));
    assertFalse(stringEquals(null).test("xo"));
    assertFalse(stringEquals("xo").test("he"));
  }

  @Test
  void testIsDate() {
    assertTrue(isDate("dd-MM-uuuu").test("28-02-2019"));
    assertTrue(isDate("dd-MM-uuuu").test("29-02-2020"));
    assertFalse(isDate("dd-MM-yyyy").test("32-02-2020"));
    assertFalse(isDate("dd-MM-yyyy").test("29-02-2019"));
    assertFalse(isDate("dd-MM-yyyy").test("31-02-2019"));
    assertFalse(isDate("dd-MM-yyyy").test("28022019"));
    assertFalse(isDate("dd-MM-yyyy").test(null));
    assertFalse(isDate(null).test("28-02-2019"));
    assertFalse(isDate("HH-MM-yyyy").test("28-02-2019"));
  }

  @Test
  void testObjectIsDate() {
    assertTrue(isDate(ObjectFrom<String>::getSource, "dd-MM-uuuu")
        .test(new ObjectFrom<>("28-02-2019", null)));
    assertFalse(isDate(ObjectFrom<String>::getSource, "dd-MM-yyyy")
        .test(new ObjectFrom<>("28022019", null)));
    assertFalse(isDate(ObjectFrom<String>::getSource, "dd-MM-yyyy").test(null));
    assertFalse(
        isDate(ObjectFrom<String>::getSource, null).test(new ObjectFrom<>("28-02-2019", null)));
    assertFalse(isDate(ObjectFrom<String>::getSource, "HH-MM-yyyy")
        .test(new ObjectFrom<>("28-02-2019", null)));
  }

  @Test
  void testIsTime() {
    assertTrue(isTime("HH:mm:ss").test("23:59:59"));
    assertFalse(isTime("HH:mm:ss").test("235959"));
    assertFalse(isTime("HH:mm:ss").test(null));
    assertFalse(isTime(null).test("23:59:59"));
    assertFalse(isTime("çç:mm:ss").test("23:59:59"));
  }

  @Test
  void testObjectIsTime() {
    assertTrue(
        isTime(ObjectFrom<String>::getSource, "HH:mm:ss").test(new ObjectFrom<>("23:59:59", null)));
    assertFalse(
        isTime(ObjectFrom<String>::getSource, "HH:mm:ss").test(new ObjectFrom<>("235959", null)));
    assertFalse(isTime(ObjectFrom<String>::getSource, "HH:mm:ss").test(null));
    assertFalse(
        isTime(ObjectFrom<String>::getSource, null).test(new ObjectFrom<>("23:59:59", null)));
    assertFalse(
        isTime(ObjectFrom<String>::getSource, "çç:mm:ss").test(new ObjectFrom<>("23:59:59", null)));
  }

  @Test
  void testIsDateTime() {
    assertTrue(isDateTime("dd-MM-uuuu HH:mm:ss").test("28-02-2019 23:59:59"));
    assertFalse(isDateTime("dd-MM-yyyy HH:mm:ss").test("28-02-2019 235959"));
    assertFalse(isDateTime("dd-MM-yyyy HH:mm:ss").test(null));
    assertFalse(isDateTime(null).test("23:59:59"));
    assertFalse(isDateTime("BB-MM-yyyy HH:mm:ss").test("28-02-2019 23:59:59"));
  }

  @Test
  void testObjectIsDateTime() {
    assertTrue(isDateTime(ObjectFrom<String>::getSource, "dd-MM-uuuu HH:mm:ss")
        .test(new ObjectFrom<>("28-02-2019 23:59:59", null)));
    assertFalse(isDateTime(ObjectFrom<String>::getSource, "dd-MM-yyyy HH:mm:ss")
        .test(new ObjectFrom<>("28-02-2019 235959", null)));
    assertFalse(isDateTime(ObjectFrom<String>::getSource, "dd-MM-yyyy HH:mm:ss").test(null));
    assertFalse(isDateTime(ObjectFrom<String>::getSource, null)
        .test(new ObjectFrom<>("28-02-2019 23:59:59", null)));
    assertFalse(isDateTime(ObjectFrom<String>::getSource, "BB-MM-yyyy HH:mm:ss")
        .test(new ObjectFrom<>("28-02-2019 23:59:59", null)));
  }

  @Test
  void testIsAlpha() {
    assertTrue(isAlpha().test("ABCDEF"));
    assertTrue(isAlpha().test("abcdef"));
    assertFalse(isAlpha().test(""));
    assertFalse(isAlpha().test("0"));
    assertFalse(isAlpha().test("0.00"));
    assertFalse(isAlpha().test("123"));
    assertFalse(isAlpha().test("-123"));
    assertFalse(isAlpha().test("1.23E3"));
    assertFalse(isAlpha().test("1.23E+3"));
    assertFalse(isAlpha().test("12.3E+7"));
    assertFalse(isAlpha().test("12.0"));
    assertFalse(isAlpha().test("12.3"));
    assertFalse(isAlpha().test("0.00123"));
    assertFalse(isAlpha().test("-1.23E-12"));
    assertFalse(isAlpha().test("1234.5E-4"));
    assertFalse(isAlpha().test("0E+7"));
    assertFalse(isAlpha().test("-0"));
  }

  @Test
  void testIsAlphaNumeric() {
    assertTrue(isAlphaNumeric().test("ABCDEF"));
    assertTrue(isAlphaNumeric().test("abcdef"));
    assertTrue(isAlphaNumeric().test("abcdef123456"));
    assertFalse(isAlphaNumeric().test(""));
    assertTrue(isAlphaNumeric().test("0"));
    assertFalse(isAlphaNumeric().test("0.00"));
    assertTrue(isAlphaNumeric().test("123"));
    assertFalse(isAlphaNumeric().test("-123"));
    assertFalse(isAlphaNumeric().test("1.23E3"));
    assertFalse(isAlphaNumeric().test("1.23E+3"));
    assertFalse(isAlphaNumeric().test("12.3E+7"));
    assertFalse(isAlphaNumeric().test("12.0"));
    assertFalse(isAlphaNumeric().test("12.3"));
    assertFalse(isAlphaNumeric().test("0.00123"));
    assertFalse(isAlphaNumeric().test("-1.23E-12"));
    assertFalse(isAlphaNumeric().test("1234.5E-4"));
    assertFalse(isAlphaNumeric().test("0E+7"));
    assertFalse(isAlphaNumeric().test("-0"));
  }

  @Test
  void testIsNumber() {
    assertFalse(isNumeric().test("ABCDEF"));
    assertFalse(isNumber().test(""));
    assertTrue(isNumber().test("0"));
    assertTrue(isNumber().test("0.00"));
    assertTrue(isNumber().test("123"));
    assertTrue(isNumber().test("-123"));
    assertTrue(isNumber().test("1.23E3"));
    assertTrue(isNumber().test("1.23E+3"));
    assertTrue(isNumber().test("12.3E+7"));
    assertTrue(isNumber().test("12.0"));
    assertTrue(isNumber().test("12.3"));
    assertTrue(isNumber().test("0.00123"));
    assertTrue(isNumber().test("-1.23E-12"));
    assertTrue(isNumber().test("1234.5E-4"));
    assertTrue(isNumber().test("0E+7"));
    assertTrue(isNumber().test("-0"));
    assertFalse(isNumber().test("1234.5E-4EE"));
  }

  @Test
  void testIsNumeric() {
    assertFalse(isNumeric().test("ABCDEF"));
    assertFalse(isNumeric().test(""));
    assertTrue(isNumeric().test("0"));
    assertFalse(isNumeric().test("0.00"));
    assertTrue(isNumeric().test("123"));
    assertFalse(isNumeric().test("-123"));
    assertFalse(isNumeric().test("1.23E3"));
    assertFalse(isNumeric().test("1.23E+3"));
    assertFalse(isNumeric().test("12.3E+7"));
    assertFalse(isNumeric().test("12.0"));
    assertFalse(isNumeric().test("12.3"));
    assertFalse(isNumeric().test("0.00123"));
    assertFalse(isNumeric().test("-1.23E-12"));
    assertFalse(isNumeric().test("1234.5E-4"));
    assertFalse(isNumeric().test("0E+7"));
    assertFalse(isNumeric().test("-0"));
  }

  @Test
  void testNullEquals() {
    assertFalse(stringEquals("xo").test((String) null));
    assertFalse(stringEquals(null).test((String) null));
    assertFalse(stringEquals(null).test("xo"));
  }

  @Test
  void testNullIsAlpha() {
    assertFalse(isAlpha().test((String) null));
    assertFalse(isAlpha().test(null));
  }

  @Test
  void testNullIsAlphaNumeric() {
    assertFalse(isAlphaNumeric().test((String) null));
    assertFalse(isAlphaNumeric().test(null));
  }

  @Test
  void testNullIsNumber() {
    assertFalse(isNumber().test((String) null));
    assertFalse(isNumber().test(null));
  }

  @Test
  void testNullIsNumeric() {
    assertFalse(isNumeric().test((String) null));
    assertFalse(isNumeric().test(null));
  }

  @Test
  void testNullMatches() {
    assertFalse(stringMatches("xo").test((String) null));
    assertFalse(stringMatches(null).test((String) null));
    assertFalse(stringMatches(null).test("xo"));
  }

  @Test
  void testNullObjectEquals() {
    assertFalse(stringEquals(ObjectFrom<String>::getSource, "xo").test(null));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, (String) null)
        .test(new ObjectFrom<>("helo", null)));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, (String) null)
        .test(new ObjectFrom<>(null, null)));
    assertFalse(
        stringEquals(ObjectFrom<String>::getSource, "xo").test(new ObjectFrom<>(null, null)));
  }

  @Test
  void testNullObjectEquals2() {
    assertFalse(
        stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>("helo", null)));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>(null, "hello")));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>(null, null)));
  }

  @Test
  void testNullObjectStringSize() {
    assertFalse(
        stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
    assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSize2() {
    assertFalse(stringSize(ObjectFrom<String>::getSource, 5).test(null));
    assertFalse(stringSize(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(
        stringSize(ObjectFrom<String>::getSource, 5).test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSize(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeGreaterThan() {
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(null));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeGreaterThan2() {
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, 5).test(null));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeGreaterThanOrEqual() {
    assertFalse(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(null));
    assertFalse(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hello", null)));
    assertFalse(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeGreaterThanOrEqual2() {
    assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, 5).test(null));
    assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeLessThan() {
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(null));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeLessThan2() {
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, 5).test(null));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeLessThanOrEqual() {
    assertFalse(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(null));
    assertFalse(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hello", null)));
    assertFalse(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullObjectStringSizeLessThanOrEqual2() {
    assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, 5).test(null));
    assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>(null, "hello")));
    assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, (Integer) null)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testNullStringSize() {
    assertFalse(stringSize(10).test((String) null));
    assertFalse(stringSize(10).test(null));
  }

  @Test
  void testNulltStringContains() {
    assertFalse(stringContains("xo").test(null));
    assertFalse(stringContains("xo").test((String) null));
    assertFalse(stringContains(null).test(null));
    assertFalse(stringContains(null).test((String) null));
    assertFalse(stringContains((String) null).test((String) null));
    assertFalse(stringContains((String) null).test(null));
    assertFalse(stringContains((String) null).test("xo"));
    assertFalse(stringContains(null).test("xo"));
  }

  @Test
  void testNulltStringEqualsIgnoreCase() {
    assertFalse(stringEqualsIgnoreCase("xo").test(null));
    assertFalse(stringEqualsIgnoreCase("xo").test((String) null));
    assertFalse(stringEqualsIgnoreCase(null).test(null));
    assertFalse(stringEqualsIgnoreCase(null).test((String) null));
    assertFalse(stringEqualsIgnoreCase((String) null).test(null));
    assertFalse(stringEqualsIgnoreCase(null).test("xo"));
    assertFalse(stringEqualsIgnoreCase((String) null).test("xo"));
  }

  @Test
  void testNulltStringSizeBetween() {
    assertFalse(stringSizeBetween(1, 1).test(null));
    assertFalse(stringSizeBetween(null, null).test(null));
    assertFalse(stringSizeBetween(null, null).test("xo"));
    assertFalse(stringSizeBetween(1, null).test(null));
    assertFalse(stringSizeBetween(null, 1).test("xo"));
    assertFalse(stringSizeBetween(null, 1).test(null));
    assertFalse(stringSizeBetween(1, null).test("xo"));
  }

  @Test
  void testNulltStringSizeGreaterThan() {
    assertFalse(stringSizeGreaterThan(1).test(null));
    assertFalse(stringSizeGreaterThan(null).test(null));
    assertFalse(stringSizeGreaterThan(null).test("xo"));
  }

  @Test
  void testNulltStringSizeGreaterThanOrEqual() {
    assertFalse(stringSizeGreaterThanOrEqual(1).test(null));
    assertFalse(stringSizeGreaterThanOrEqual(null).test(null));
    assertFalse(stringSizeGreaterThanOrEqual(null).test("xo"));
  }

  @Test
  void testNulltStringSizeLessThan() {
    assertFalse(stringSizeLessThan(1).test(null));
    assertFalse(stringSizeLessThan(null).test(null));
    assertFalse(stringSizeLessThan(null).test("xo"));
  }

  @Test
  void testNulltStringSizeLessThanOrEqual() {
    assertFalse(stringSizeLessThanOrEqual(1).test(null));
    assertFalse(stringSizeLessThanOrEqual(null).test(null));
    assertFalse(stringSizeLessThanOrEqual(null).test("xo"));
  }

  @Test
  void testObjectEqual2() {
    assertTrue(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>("xo", "xo")));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>("xo", "Xo")));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>("xo", null)));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>(null, "xo")));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<>(null, null)));
  }

  @Test
  void testObjectEquals() {
    assertTrue(
        stringEquals(ObjectFrom<String>::getSource, "xo").test(new ObjectFrom<>("xo", null)));
    assertFalse(
        stringEquals(ObjectFrom<String>::getSource, "xo").test(new ObjectFrom<>("Xo", null)));
    assertFalse(
        stringEquals(ObjectFrom<String>::getSource, "xo").test(new ObjectFrom<>(null, null)));
    assertFalse(stringEquals(ObjectFrom<String>::getSource, (String) null)
        .test(new ObjectFrom<>("xo", null)));
    assertFalse(
        stringEquals(ObjectFrom<String>::getSource, "xo").test(new ObjectFrom<>("he", null)));
  }

  @Test
  void testObjectIsAlpha() {
    assertTrue(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("ABCDEF", null)));
    assertTrue(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("abcdef", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0.00", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("123", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-123", null)));
    assertFalse(
        isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1.23E3", null)));
    assertFalse(
        isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1.23E+3", null)));
    assertFalse(
        isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.3E+7", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.0", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.3", null)));
    assertFalse(
        isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0.00123", null)));
    assertFalse(
        isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-1.23E-12", null)));
    assertFalse(
        isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1234.5E-4", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0E+7", null)));
    assertFalse(isAlpha(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-0", null)));
  }

  @Test
  void testObjectIsAlphaNumeric() {
    assertTrue(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("ABCDEF", null)));
    assertTrue(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("abcdef", null)));
    assertTrue(isAlphaNumeric(ObjectFrom<String>::getSource)
        .test(new ObjectFrom<String>("abcdef123456", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("", null)));
    assertTrue(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0.00", null)));
    assertTrue(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("123", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-123", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1.23E3", null)));
    assertFalse(isAlphaNumeric(ObjectFrom<String>::getSource)
        .test(new ObjectFrom<String>("1.23E+3", null)));
    assertFalse(isAlphaNumeric(ObjectFrom<String>::getSource)
        .test(new ObjectFrom<String>("12.3E+7", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.0", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.3", null)));
    assertFalse(isAlphaNumeric(ObjectFrom<String>::getSource)
        .test(new ObjectFrom<String>("0.00123", null)));
    assertFalse(isAlphaNumeric(ObjectFrom<String>::getSource)
        .test(new ObjectFrom<String>("-1.23E-12", null)));
    assertFalse(isAlphaNumeric(ObjectFrom<String>::getSource)
        .test(new ObjectFrom<String>("1234.5E-4", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0E+7", null)));
    assertFalse(
        isAlphaNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-0", null)));
  }

  @Test
  void testObjectIsNumber() {
    assertFalse(
        isNumeric(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("ABCDEF", null)));
    assertFalse(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0.00", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("123", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-123", null)));
    assertTrue(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1.23E3", null)));
    assertTrue(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1.23E+3", null)));
    assertTrue(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.3E+7", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.0", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("12.3", null)));
    assertTrue(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0.00123", null)));
    assertTrue(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-1.23E-12", null)));
    assertTrue(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1234.5E-4", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("0E+7", null)));
    assertTrue(isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("-0", null)));
    assertFalse(
        isNumber(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("1234.5E-4EE", null)));
  }

  @Test
  void testObjectNullStringEqualsIgnoreCase() {
    assertFalse(stringEqualsIgnoreCase(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(null));
    assertFalse(stringEqualsIgnoreCase(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>(null, null)));
  }

  @Test
  void testObjectNulltStringSizeBetween() {
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, 1, 1).test(null));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, null, null).test(null));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, null, null)
        .test(new ObjectFrom<String>("xo", null)));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, 1, null).test(null));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, null, 1)
        .test(new ObjectFrom<String>("xo", null)));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, null, 1).test(null));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, 1, null)
        .test(new ObjectFrom<String>("xo", null)));
  }

  @Test
  void testObjectStringContains() {
    assertTrue(stringContains(ObjectFrom<String>::getSource, "lo")
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringContains(ObjectFrom<String>::getSource, "xo")
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringContains(ObjectFrom<String>::getSource, "xo")
        .test(new ObjectFrom<String>(null, null)));
    assertFalse(stringContains(ObjectFrom<String>::getSource, "xo").test(null));
  }

  @Test
  void testObjectStringEmptyOrNull() {
    assertTrue(
        stringEmptyOrNull(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("", null)));
    assertTrue(
        stringEmptyOrNull(ObjectFrom<String>::getSource).test(new ObjectFrom<String>(null, null)));
    assertFalse(
        stringEmptyOrNull(ObjectFrom<String>::getSource).test(new ObjectFrom<String>("o", null)));
  }

  @Test
  void testObjectStringEqualsIgnoreCase() {
    assertTrue(stringEqualsIgnoreCase(ObjectFrom<String>::getSource, "hello")
        .test(new ObjectFrom<String>("HeLlO", null)));
    assertTrue(stringEqualsIgnoreCase(ObjectFrom<String>::getSource, "hello")
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringEqualsIgnoreCase(ObjectFrom<String>::getSource, "xo")
        .test(new ObjectFrom<String>("xoo", null)));
  }

  @Test
  void testObjectStringMatches() {
    assertTrue(stringMatches(ObjectFrom<String>::getSource, "^h.*o$")
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringMatches(ObjectFrom<String>::getSource, "^x$")
        .test(new ObjectFrom<String>("hello", null)));
    assertFalse(stringMatches(ObjectFrom<String>::getSource, "^x$")
        .test(new ObjectFrom<String>(null, null)));
    assertFalse(stringMatches(ObjectFrom<String>::getSource, "^x$").test(null));
  }

  @Test
  void testObjectStringSize() {
    assertTrue(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hell", "hello")));
    assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", "hell")));
  }

  @Test
  void testObjectStringSize2() {
    assertTrue(
        stringSize(ObjectFrom<String>::getSource, 5).test(new ObjectFrom<>("hello", "hello")));
    assertFalse(
        stringSize(ObjectFrom<String>::getSource, 5).test(new ObjectFrom<>("hell", "hello")));
    assertFalse(
        stringSize(ObjectFrom<String>::getSource, 4).test(new ObjectFrom<>("hello", "hell")));
  }

  @Test
  void testObjectStringSizeBetween() {
    assertTrue(stringSizeBetween(ObjectFrom<String>::getSource, 0, 6)
        .test(new ObjectFrom<String>("hell", "hello")));
    assertTrue(stringSizeBetween(ObjectFrom<String>::getSource, 0, 5)
        .test(new ObjectFrom<String>("hell", "hello")));
    assertFalse(stringSizeBetween(ObjectFrom<String>::getSource, 6, 0)
        .test(new ObjectFrom<String>("hell", "hello")));
  }

  @Test
  void testObjectStringSizeGreaterThan() {
    assertTrue(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", "hell")));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hell", "hello")));
  }

  @Test
  void testObjectStringSizeGreaterThan2() {
    assertTrue(stringSizeGreaterThan(ObjectFrom<String>::getSource, 4)
        .test(new ObjectFrom<String>("hello", "hell")));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hell", "hello")));
  }

  @Test
  void testObjectStringSizeGreaterThanOrEqual() {
    assertTrue(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hello", "hell")));
    assertTrue(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(
        stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hell", "hello")));
  }

  @Test
  void testObjectStringSizeGreaterThanOrEqual2() {
    assertTrue(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, 4)
        .test(new ObjectFrom<String>("hello", "hell")));
    assertTrue(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hell", "hello")));
  }

  @Test
  void testObjectStringSizeLessThan() {
    assertTrue(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hell", "hello")));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
        .test(new ObjectFrom<String>("hello", "hell")));
  }

  @Test
  void testObjectStringSizeLessThan2() {
    assertTrue(stringSizeLessThan(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hell", "hello")));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, 4)
        .test(new ObjectFrom<String>("hello", "hell")));
  }

  @Test
  void testObjectStringSizeLessThanOrEqual() {
    assertTrue(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hell", "hello")));
    assertTrue(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(
        stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget)
            .test(new ObjectFrom<String>("hello", "hell")));
  }

  @Test
  void testObjectStringSizeLessThanOrEqual2() {
    assertTrue(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hell", "hello")));
    assertTrue(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, 5)
        .test(new ObjectFrom<String>("hello", "hello")));
    assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, 4)
        .test(new ObjectFrom<String>("hello", "hell")));
  }

  @Test
  void testStringContains() {
    assertTrue(stringContains("lo").test("hello"));
    assertFalse(stringContains("xo").test("hello"));
  }

  @Test
  void testStringEmptyOrNull() {
    assertTrue(stringEmptyOrNull().test(""));
    assertTrue(stringEmptyOrNull().test(null));
    assertTrue(stringEmptyOrNull().test((String) null));
    assertFalse(stringEmptyOrNull().test("o"));
  }

  @Test
  void testStringEqualsIgnoreCase() {
    assertTrue(stringEqualsIgnoreCase("hello").test("HeLlo"));
    assertTrue(stringEqualsIgnoreCase("hello").test("hello"));
    assertFalse(stringEqualsIgnoreCase("hello").test("hell"));
  }

  @Test
  void testStringMatches() {
    assertTrue(stringMatches("^[0-9]{1,15}\\.[0-9]{2}$").test("1200.00"));
    assertTrue(stringMatches("^h.*o$").test("hello"));
    assertFalse(stringMatches("^x$").test("hello"));
    assertFalse(stringMatches("^[0-9]{1,15}\\.[0-9]{2}$").test("1200"));
  }

  @Test
  void testStringSize() {
    assertTrue(stringSize(2).test("he"));
    assertFalse(stringSize(1).test("he"));
  }

  @Test
  void testStringSizeBetween() {
    assertTrue(stringSizeBetween(0, 6).test("hello"));
    assertTrue(stringSizeBetween(0, 5).test("hello"));
    assertFalse(stringSizeBetween(6, 0).test("hello"));
  }

  @Test
  void testStringSizeGreaterThan() {
    assertTrue(stringSizeGreaterThan(1).test("he"));
    assertFalse(stringSizeGreaterThan(2).test("he"));
  }

  @Test
  void testStringSizeGreaterThanOrEqual() {
    assertTrue(stringSizeGreaterThanOrEqual(2).test("he"));
    assertFalse(stringSizeGreaterThanOrEqual(3).test("he"));
  }

  @Test
  void testStringSizeLessThan() {
    assertTrue(stringSizeLessThan(6).test("hello"));
    assertFalse(stringSizeLessThan(5).test("hello"));
  }

  @Test
  void testStringSizeLessThanOrEqual() {
    assertTrue(stringSizeLessThanOrEqual(5).test("hello"));
    assertFalse(stringSizeLessThanOrEqual(4).test("hello"));
  }

  @Test
  void testStringInCollectionUsingList() {
    final List<String> list = new ArrayList<>(2);
    list.add("foo");
    list.add("bar");

    assertTrue(stringInCollection(list).test("foo"));
    assertFalse(stringInCollection(list).test("test"));
    assertFalse(stringInCollection(list).test(""));

    assertTrue(stringInCollection(CollectionTestObject::getSource, list).test(new CollectionTestObject("foo", list)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, list).test(new CollectionTestObject("test", list)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, list).test(new CollectionTestObject("", list)));

    assertTrue(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test (new CollectionTestObject("foo", list)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("test", list)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("", list)));

    assertTrue(stringInCollection("foo", CollectionTestObject::getTarget).test (new CollectionTestObject("foo", list)));
    assertFalse(stringInCollection("test", CollectionTestObject::getTarget).test(new CollectionTestObject("test", list)));
    assertFalse(stringInCollection("", CollectionTestObject::getTarget).test(new CollectionTestObject("", list)));
  }

  @Test
  void testStringInCollectionUsingSet() {
    final Set<String> set = new HashSet<>(2);
    set.add("foo");
    set.add("bar");

    assertTrue(stringInCollection(set).test("foo"));
    assertFalse(stringInCollection(set).test("test"));
    assertFalse(stringInCollection(set).test(""));

    assertTrue(stringInCollection(CollectionTestObject::getSource, set).test(new CollectionTestObject("foo", set)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, set).test(new CollectionTestObject("test", set)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, set).test(new CollectionTestObject("", set)));

    assertTrue(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test (new CollectionTestObject("foo", set)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("test", set)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("", set)));

    assertTrue(stringInCollection("foo", CollectionTestObject::getTarget).test (new CollectionTestObject("foo", set)));
    assertFalse(stringInCollection("test", CollectionTestObject::getTarget).test(new CollectionTestObject("test", set)));
    assertFalse(stringInCollection("", CollectionTestObject::getTarget).test(new CollectionTestObject("", set)));
  }

  @Test
  void testStringInCollectionUsingQueue() {
    final Queue<String> queue = new PriorityQueue<>(2);
    queue.add("foo");
    queue.add("bar");

    assertTrue(stringInCollection(queue).test("foo"));
    assertFalse(stringInCollection(queue).test("test"));
    assertFalse(stringInCollection(queue).test(""));

    assertTrue(stringInCollection(CollectionTestObject::getSource, queue).test(new CollectionTestObject("foo", queue)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, queue).test(new CollectionTestObject("test", queue)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, queue).test(new CollectionTestObject("", queue)));

    assertTrue(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test (new CollectionTestObject("foo", queue)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("test", queue)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("", queue)));

    assertTrue(stringInCollection("foo", CollectionTestObject::getTarget).test (new CollectionTestObject("foo", queue)));
    assertFalse(stringInCollection("test", CollectionTestObject::getTarget).test(new CollectionTestObject("test", queue)));
    assertFalse(stringInCollection("", CollectionTestObject::getTarget).test(new CollectionTestObject("", queue)));
  }

  @Test
  void testNullValuesForStringInCollectionUsingList() {
    final List<String> list = new ArrayList<>(2);
    list.add("foo");
    list.add("bar");

    assertFalse(stringInCollection(list).test(null));

    assertFalse(stringInCollection(CollectionTestObject::getSource, list).test(new CollectionTestObject(null, list)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, list).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Collection<String>) null).test(new CollectionTestObject("foo", list)));
    assertFalse(stringInCollection(null, list).test(new CollectionTestObject(null, list)));
    assertFalse(stringInCollection(null, list).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection(null, (Collection<String>) null).test(new CollectionTestObject(null, list)));

    assertFalse(stringInCollection("foo", CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection("foo", null).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((String) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, list)));
    assertFalse(stringInCollection((String) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((String) null, null).test(new CollectionTestObject(null, null)));

    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject(null, list)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, list)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, CollectionTestObject::getTarget).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject(null, list)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject(null, list)));
  }

  @Test
  void testNullValuesForStringInCollectionUsingSet() {
    final Set<String> set = new HashSet<>(2);
    set.add("foo");
    set.add("bar");

    assertFalse(stringInCollection(set).test(null));

    assertFalse(stringInCollection(CollectionTestObject::getSource, set).test(new CollectionTestObject(null, set)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, set).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Collection<String>) null).test(new CollectionTestObject("foo", set)));
    assertFalse(stringInCollection(null, set).test(new CollectionTestObject(null, set)));
    assertFalse(stringInCollection(null, set).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection(null, (Collection<String>) null).test(new CollectionTestObject(null, set)));

    assertFalse(stringInCollection("foo", CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection("foo", null).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((String) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, set)));
    assertFalse(stringInCollection((String) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((String) null, null).test(new CollectionTestObject(null, null)));

    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject(null, set)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, set)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, CollectionTestObject::getTarget).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject(null, set)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject(null, set)));
  }

  @Test
  void testNullValuesForStringInCollectionUsingQueue() {
    final Queue<String> queue = new PriorityQueue<>(2);
    queue.add("foo");
    queue.add("bar");

    assertFalse(stringInCollection(queue).test(null));

    assertFalse(stringInCollection(CollectionTestObject::getSource, queue).test(new CollectionTestObject(null, queue)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, queue).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Collection<String>) null).test(new CollectionTestObject("foo", queue)));
    assertFalse(stringInCollection(null, queue).test(new CollectionTestObject(null, queue)));
    assertFalse(stringInCollection(null, queue).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection(null, (Collection<String>) null).test(new CollectionTestObject(null, queue)));

    assertFalse(stringInCollection("foo", CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection("foo", null).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((String) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, queue)));
    assertFalse(stringInCollection((String) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((String) null, null).test(new CollectionTestObject(null, null)));

    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject(null, queue)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, CollectionTestObject::getTarget).test(new CollectionTestObject(null, null)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, CollectionTestObject::getTarget).test(new CollectionTestObject(null, queue)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, CollectionTestObject::getTarget).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject("foo", null)));
    assertFalse(stringInCollection(CollectionTestObject::getSource, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject(null, queue)));
    assertFalse(stringInCollection((Function<CollectionTestObject, String>) null, (Function<CollectionTestObject, Collection<String>>) null).test(new CollectionTestObject(null, queue)));
  }

  private static class CollectionTestObject extends ComplexObjectFrom<String, Collection<String>> {

    public CollectionTestObject(String source, Collection<String> target) {
      super(source, target);
    }

  }

}
