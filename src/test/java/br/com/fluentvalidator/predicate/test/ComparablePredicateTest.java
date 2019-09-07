package br.com.fluentvalidator.predicate.test;

import static br.com.fluentvalidator.predicate.ComparablePredicate.between;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThanOrEqual;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;

public class ComparablePredicateTest {

	@Test
	public void testNullComparableGreaterThan() {
		assertFalse(greaterThan(1).test(null));
	}

	@Test
	public void testNullComparableLessThan() {
		assertFalse(lessThan(1).test(null));
	}

	@Test
	public void testNullComparableGreaterThanOrEqual() {
		assertFalse(greaterThanOrEqual(1).test(null));
	}

	@Test
	public void testNullComparableLessThanOrEqual() {
		assertFalse(lessThanOrEqual(1).test(null));
	}

	@Test
	public void testNullComparableBetween() {
		assertFalse(between(1, 1).test(null));
	}

	@Test
	public void testComparableIntegerGreaterThan() {
		assertTrue(greaterThan(1).test(2));
		assertFalse(greaterThan(2).test(2));
	}

	@Test
	public void testComparableIntegerLessThan() {
		assertTrue(lessThan(6).test(5));
		assertFalse(lessThan(5).test(5));
	}
	
	@Test
	public void testComparableIntegerGreaterThanOrEqual() {
		assertTrue(greaterThanOrEqual(2).test(2));
		assertTrue(greaterThanOrEqual(2).test(3));
		assertFalse(greaterThanOrEqual(3).test(2));
	}

	@Test
	public void testComparableIntegerLessThanOrEqual() {
		assertTrue(lessThanOrEqual(5).test(5));
		assertTrue(lessThanOrEqual(5).test(4));
		assertFalse(lessThanOrEqual(4).test(5));
	}

	@Test
	public void testComparableIntegerBetween() {
		assertTrue(between(0, 5).test(2));
		assertFalse(between(5, 0).test(5));
		assertFalse(between(0, 0).test(5));
	}

	@Test
	public void testComparableLongPredicates() {
		assertTrue(greaterThan(1L).test(2L));
		assertFalse(greaterThan(2L).test(2L));

		assertTrue(lessThan(6L).test(5L));
		assertFalse(lessThan(5L).test(5L));

		assertTrue(greaterThanOrEqual(2L).test(2L));
		assertFalse(greaterThanOrEqual(3L).test(2L));

		assertTrue(lessThanOrEqual(5L).test(5L));
		assertFalse(lessThanOrEqual(4L).test(5L));

		assertTrue(between(0L, 5L).test(2L));
		assertFalse(between(5L, 0L).test(6L));
	}

	@Test
	public void testComparableDoublePredicates() {
		assertTrue(greaterThan(1d).test(2d));
		assertFalse(greaterThan(2d).test(2d));

		assertTrue(lessThan(6d).test(5d));
		assertFalse(lessThan(5d).test(5d));

		assertTrue(greaterThanOrEqual(2d).test(2d));
		assertFalse(greaterThanOrEqual(3d).test(2d));

		assertTrue(lessThanOrEqual(5d).test(5d));
		assertFalse(lessThanOrEqual(4d).test(5d));

		assertTrue(between(0d, 5d).test(2d));
		assertFalse(between(5d, 0d).test(6d));
	}

	@Test
	public void testComparableFloatPredicates() {
		assertTrue(greaterThan(1f).test(2f));
		assertFalse(greaterThan(2f).test(2f));

		assertTrue(lessThan(6f).test(5f));
		assertFalse(lessThan(5f).test(5f));

		assertTrue(greaterThanOrEqual(2f).test(2f));
		assertFalse(greaterThanOrEqual(3f).test(2f));

		assertTrue(lessThanOrEqual(5f).test(5f));
		assertFalse(lessThanOrEqual(4f).test(5f));

		assertTrue(between(0f, 5f).test(2f));
		assertFalse(between(5f, 0f).test(6f));
	}

	@Test
	public void testComparableBigIntegerPredicates() {
		assertTrue(greaterThan(BigInteger.valueOf(1)).test(BigInteger.valueOf(2)));
		assertFalse(greaterThan(BigInteger.valueOf(2)).test(BigInteger.valueOf(2)));

		assertTrue(lessThan(BigInteger.valueOf(6)).test(BigInteger.valueOf(5)));
		assertFalse(lessThan(BigInteger.valueOf(5)).test(BigInteger.valueOf(5)));

		assertTrue(greaterThanOrEqual(BigInteger.valueOf(2)).test(BigInteger.valueOf(2)));
		assertFalse(greaterThanOrEqual(BigInteger.valueOf(3)).test(BigInteger.valueOf(2)));

		assertTrue(lessThanOrEqual(BigInteger.valueOf(5)).test(BigInteger.valueOf(5)));
		assertFalse(lessThanOrEqual(BigInteger.valueOf(4)).test(BigInteger.valueOf(5)));

		assertTrue(between(BigInteger.valueOf(0), BigInteger.valueOf(5)).test(BigInteger.valueOf(2)));
		assertFalse(between(BigInteger.valueOf(5), BigInteger.valueOf(0)).test(BigInteger.valueOf(6)));
	}

	@Test
	public void testComparableBigDecimalPredicates() {
		assertTrue(greaterThan(BigDecimal.valueOf(1)).test(BigDecimal.valueOf(2)));
		assertFalse(greaterThan(BigDecimal.valueOf(2)).test(BigDecimal.valueOf(2)));

		assertTrue(lessThan(BigDecimal.valueOf(6)).test(BigDecimal.valueOf(5)));
		assertFalse(lessThan(BigDecimal.valueOf(5)).test(BigDecimal.valueOf(5)));

		assertTrue(greaterThanOrEqual(BigDecimal.valueOf(2)).test(BigDecimal.valueOf(2)));
		assertFalse(greaterThanOrEqual(BigDecimal.valueOf(3)).test(BigDecimal.valueOf(2)));

		assertTrue(lessThanOrEqual(BigDecimal.valueOf(5)).test(BigDecimal.valueOf(5)));
		assertFalse(lessThanOrEqual(BigDecimal.valueOf(4)).test(BigDecimal.valueOf(5)));

		assertTrue(between(BigDecimal.valueOf(0), BigDecimal.valueOf(5)).test(BigDecimal.valueOf(2)));
		assertFalse(between(BigDecimal.valueOf(5), BigDecimal.valueOf(0)).test(BigDecimal.valueOf(6)));
	}

}
