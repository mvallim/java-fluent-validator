package br.com.fluentvalidator.predicate.test;

import static br.com.fluentvalidator.predicate.StringPredicate.stringMatches;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeBetween;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeGreaterThan;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeGreaterThanOrEqual;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThan;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThanOrEqual;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class StringPredicateTest {

	@Test
	public void testNulltStringSizeGreaterThan() {
		try {
			stringSizeGreaterThan(1).test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringSizeGreaterThan' could not evaluate null value"));
		}
	}

	@Test
	public void testNulltStringSizeLessThan() {
		try {
			stringSizeLessThan(1).test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringSizeLessThan' could not evaluate null value"));
		}
	}

	@Test
	public void testNulltStringSizeGreaterThanOrEqual() {
		try {
			stringSizeGreaterThanOrEqual(1).test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringSizeGreaterThanOrEqual' could not evaluate null value"));
		}
	}

	@Test
	public void testNulltStringSizeLessThanOrEqual() {
		try {
			stringSizeLessThanOrEqual(1).test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringSizeLessThanOrEqual' could not evaluate null value"));
		}
	}

	@Test
	public void testNulltStringSizeBetween() {
		try {
			stringSizeBetween(1, 1).test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringSizeBetween' could not evaluate null value"));
		}
	}

	@Test
	public void testNulltStringContains() {
		try {
			stringContains("xo").test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringContains' could not evaluate null value"));
		}
	}

	@Test
	public void testNullMatches() {
		try {
			stringMatches("xo").test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), equalTo("predicate 'stringMatches' could not evaluate null value"));
		}
	}

	@Test
	public void testStringPredicates() {

		assertTrue(stringSizeGreaterThan(1).test("he"));
		assertFalse(stringSizeGreaterThan(2).test("he"));

		assertTrue(stringSizeLessThan(6).test("hello"));
		assertFalse(stringSizeLessThan(5).test("hello"));

		assertTrue(stringSizeGreaterThanOrEqual(2).test("he"));
		assertFalse(stringSizeGreaterThanOrEqual(3).test("he"));

		assertTrue(stringSizeLessThanOrEqual(5).test("hello"));
		assertFalse(stringSizeLessThanOrEqual(4).test("hello"));

		assertTrue(stringSizeBetween(0, 6).test("hello"));
		assertFalse(stringSizeBetween(6, 0).test("hello"));
		assertFalse(stringSizeBetween(0, 0).test("hello"));

		assertTrue(stringContains("lo").test("hello"));
		assertFalse(stringContains("xo").test("hello"));

		assertTrue(stringMatches("^h.*o$").test("hello"));
		assertFalse(stringMatches("^x$").test("hello"));

		assertTrue(stringEmptyOrNull().test(""));
		assertTrue(stringEmptyOrNull().test(null));
		assertFalse(stringEmptyOrNull().test("o"));
	}

}
