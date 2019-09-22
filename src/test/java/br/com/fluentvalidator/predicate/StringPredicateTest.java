package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.StringPredicate.isAlpha;
import static br.com.fluentvalidator.predicate.StringPredicate.isAlphaNumeric;
import static br.com.fluentvalidator.predicate.StringPredicate.isNumber;
import static br.com.fluentvalidator.predicate.StringPredicate.isNumeric;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import static br.com.fluentvalidator.predicate.StringPredicate.stringMatches;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSize;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeBetween;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeGreaterThan;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeGreaterThanOrEqual;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThan;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThanOrEqual;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class StringPredicateTest {

    @Test
    public void testNulltStringSizeGreaterThan() {
        assertFalse(stringSizeGreaterThan(1).test(null));
        assertFalse(stringSizeGreaterThan(null).test(null));
        assertFalse(stringSizeGreaterThan(null).test("xo"));
    }

    @Test
    public void testNulltStringSizeLessThan() {
        assertFalse(stringSizeLessThan(1).test(null));
        assertFalse(stringSizeLessThan(null).test(null));
        assertFalse(stringSizeLessThan(null).test("xo"));
    }

    @Test
    public void testNulltStringSizeGreaterThanOrEqual() {
        assertFalse(stringSizeGreaterThanOrEqual(1).test(null));
        assertFalse(stringSizeGreaterThanOrEqual(null).test(null));
        assertFalse(stringSizeGreaterThanOrEqual(null).test("xo"));
    }

    @Test
    public void testNulltStringSizeLessThanOrEqual() {
        assertFalse(stringSizeLessThanOrEqual(1).test(null));
        assertFalse(stringSizeLessThanOrEqual(null).test(null));
        assertFalse(stringSizeLessThanOrEqual(null).test("xo"));
    }

    @Test
    public void testNulltStringSizeBetween() {
        assertFalse(stringSizeBetween(1, 1).test(null));
        assertFalse(stringSizeBetween(null, null).test(null));
        assertFalse(stringSizeBetween(null, null).test("xo"));
        assertFalse(stringSizeBetween(1, null).test(null));
        assertFalse(stringSizeBetween(null, 1).test("xo"));
        assertFalse(stringSizeBetween(null, 1).test(null));
        assertFalse(stringSizeBetween(1, null).test("xo"));
    }

    @Test
    public void testNulltStringContains() {
        assertFalse(stringContains("xo").test(null));
        assertFalse(stringContains(null).test(null));
        assertFalse(stringContains(null).test("xo"));
    }

    @Test
    public void testNullMatches() {
        assertFalse(stringMatches("xo").test(null));
        assertFalse(stringMatches(null).test(null));
        assertFalse(stringMatches(null).test("xo"));
    }

    @Test
    public void testNullIsNumeric() {
        assertFalse(isNumeric().test(null));
    }

    @Test
    public void testNullIsNumber() {
        assertFalse(isNumber().test(null));
    }

    @Test
    public void testNullIsAlpha() {
        assertFalse(isAlpha().test(null));
    }

    @Test
    public void testNullIsAlphaNumeric() {
        assertFalse(isAlphaNumeric().test(null));
    }

    @Test
    public void testNullStringSize() {
        assertFalse(stringSize(10).test(null));
    }

    @Test
    public void testIsNumeric() {
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
    public void testIsNumber() {
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
    public void testIsAlpha() {
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
    public void testIsAlphaNumeric() {
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
    public void testStringSize() {
        assertTrue(stringSize(2).test("he"));
        assertFalse(stringSize(1).test("he"));
    }

    @Test
    public void testStringSizeGreaterThan() {
        assertTrue(stringSizeGreaterThan(1).test("he"));
        assertFalse(stringSizeGreaterThan(2).test("he"));
    }

    @Test
    public void testStringSizeLessThan() {
        assertTrue(stringSizeLessThan(6).test("hello"));
        assertFalse(stringSizeLessThan(5).test("hello"));
    }

    @Test
    public void testStringSizeGreaterThanOrEqual() {
        assertTrue(stringSizeGreaterThanOrEqual(2).test("he"));
        assertFalse(stringSizeGreaterThanOrEqual(3).test("he"));
    }

    @Test
    public void testStringSizeLessThanOrEqual() {
        assertTrue(stringSizeLessThanOrEqual(5).test("hello"));
        assertFalse(stringSizeLessThanOrEqual(4).test("hello"));
    }

    @Test
    public void testStringSizeBetween() {
        assertTrue(stringSizeBetween(0, 6).test("hello"));
        assertTrue(stringSizeBetween(0, 5).test("hello"));
        assertFalse(stringSizeBetween(6, 0).test("hello"));
    }

    @Test
    public void testStringContains() {
        assertTrue(stringContains("lo").test("hello"));
        assertFalse(stringContains("xo").test("hello"));
    }

    @Test
    public void testStringMatches() {
        assertTrue(stringMatches("^h.*o$").test("hello"));
        assertFalse(stringMatches("^x$").test("hello"));
    }

    @Test
    public void testStringEmptyOrNull() {

        assertTrue(stringEmptyOrNull().test(""));
        assertTrue(stringEmptyOrNull().test(null));
        assertFalse(stringEmptyOrNull().test("o"));
    }

    @Test
    public void testObjectStringSize() {
        assertTrue(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hello")));
        assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hell", "hello")));
        assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hell")));
    }

    @Test
    public void testNullObjectStringSize() {
        assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
        assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", null)));
        assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, "hello")));
        assertFalse(stringSize(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, null)));
    }

    @Test
    public void testObjectStringSizeGreaterThan() {
        assertTrue(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hell")));
        assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hello")));
        assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hell", "hello")));
    }

    @Test
    public void testNullObjectStringSizeGreaterThan() {
        assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
        assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", null)));
        assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, "hello")));
        assertFalse(stringSizeGreaterThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, null)));
    }

    @Test
    public void testObjectStringSizeGreaterThanOrEqual() {
        assertTrue(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hell")));
        assertTrue(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hello")));
        assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hell", "hello")));
    }

    @Test
    public void testNullObjectStringSizeGreaterThanOrEqual() {
        assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
        assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", null)));
        assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, "hello")));
        assertFalse(stringSizeGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, null)));
    }

    @Test
    public void testObjectStringSizeLessThan() {
        assertTrue(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hell", "hello")));
        assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hello")));
        assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hell")));
    }

    @Test
    public void testNullObjectStringSizeLessThan() {
        assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
        assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", null)));
        assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, "hello")));
        assertFalse(stringSizeLessThan(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, null)));
    }

    @Test
    public void testObjectStringSizeLessThanOrEqual() {
        assertTrue(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hell", "hello")));
        assertTrue(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hello")));
        assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", "hell")));
    }

    @Test
    public void testNullObjectStringSizeLessThanOrEqual() {
        assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(null));
        assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>("hello", null)));
        assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, "hello")));
        assertFalse(stringSizeLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom<String>::getTarget).test(new ObjectFrom<String>(null, null)));
    }

}
