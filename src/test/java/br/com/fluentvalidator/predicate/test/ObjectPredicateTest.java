package br.com.fluentvalidator.predicate.test;

import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.instanceOf;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ObjectPredicateTest {

	@Test
	public void testNullObjectEqualTo() {
		try {
			equalTo("1").test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), org.hamcrest.Matchers.equalTo("predicate 'equalTo' could not evaluate null value"));
		}
	}

	@Test
	public void testNullObjectInstanceOf() {
		try {
			instanceOf(String.class).test(null);
		} catch (NullPointerException e) {
			assertThat(e.getMessage(), org.hamcrest.Matchers.equalTo("predicate 'instanceOf' could not evaluate null value"));
		}
	}

	@Test
	public void testObjectPredicates() {
		assertTrue(nullValue().test(null));
		assertFalse(nullValue().test("false"));

		assertTrue(nullValue(String.class).test(null));
		assertFalse(nullValue(String.class).test("he"));

		assertTrue(equalTo("1").test("1"));
		assertFalse(equalTo("1").test("he"));

		assertTrue(instanceOf(String.class).test("1"));
		assertFalse(instanceOf(String.class).test(1));
	}

}
