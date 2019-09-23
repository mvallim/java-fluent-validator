package br.com.fluentvalidator.predicate;

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
        assertFalse(greaterThan(null).test(null));
        assertFalse(greaterThan((Integer) null).test(1));
    }

    @Test
    public void testNullComparableLessThan() {
        assertFalse(lessThan(1).test(null));
        assertFalse(lessThan(null).test(null));
        assertFalse(lessThan((Integer) null).test(1));
    }

    @Test
    public void testNullComparableGreaterThanOrEqual() {
        assertFalse(greaterThanOrEqual(1).test(null));
        assertFalse(greaterThanOrEqual(null).test(null));
        assertFalse(greaterThanOrEqual((Integer) null).test(1));
    }

    @Test
    public void testNullComparableLessThanOrEqual() {
        assertFalse(lessThanOrEqual(1).test(null));
        assertFalse(lessThanOrEqual(null).test(null));
        assertFalse(lessThanOrEqual((Integer) null).test(1));
    }

    @Test
    public void testNullComparableBetween() {
        assertFalse(between(1, 1).test(null));
        assertFalse(greaterThan(null).test(null));
        assertFalse(greaterThan((Integer) null).test(1));
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
    public void testComparableLongGreaterThan() {
        assertTrue(greaterThan(1L).test(2L));
        assertFalse(greaterThan(2L).test(2L));
    }

    @Test
    public void testComparableLongLessThan() {
        assertTrue(lessThan(6L).test(5L));
        assertFalse(lessThan(5L).test(5L));
    }

    @Test
    public void testComparableLongGreaterThanOrEqual() {
        assertTrue(greaterThanOrEqual(2L).test(2L));
        assertFalse(greaterThanOrEqual(3L).test(2L));
    }

    @Test
    public void testComparableLongLessThanOrEqual() {
        assertTrue(lessThanOrEqual(5L).test(5L));
        assertFalse(lessThanOrEqual(4L).test(5L));
    }

    @Test
    public void testComparableLongBetween() {
        assertTrue(between(0L, 5L).test(2L));
        assertFalse(between(5L, 0L).test(6L));
    }

    @Test
    public void testComparableDoubleGreaterThan() {
        assertTrue(greaterThan(1d).test(2d));
        assertFalse(greaterThan(2d).test(2d));
    }

    @Test
    public void testComparableDoubleLessThan() {
        assertTrue(lessThan(6d).test(5d));
        assertFalse(lessThan(5d).test(5d));
    }

    @Test
    public void testComparableDoubleGreaterThanOrEqual() {
        assertTrue(greaterThanOrEqual(2d).test(2d));
        assertFalse(greaterThanOrEqual(3d).test(2d));
    }

    @Test
    public void testComparableDoubleLessThanOrEqual() {
        assertTrue(lessThanOrEqual(5d).test(5d));
        assertFalse(lessThanOrEqual(4d).test(5d));
    }

    @Test
    public void testComparableDoubleBetween() {
        assertTrue(between(0d, 5d).test(2d));
        assertFalse(between(5d, 0d).test(6d));
    }

    @Test
    public void testComparableFloatGreaterThan() {
        assertTrue(greaterThan(1f).test(2f));
        assertFalse(greaterThan(2f).test(2f));
    }

    @Test
    public void testComparableFloatLessThan() {
        assertTrue(lessThan(6f).test(5f));
        assertFalse(lessThan(5f).test(5f));
    }

    @Test
    public void testComparableFloatGreaterThanOrEqual() {
        assertTrue(greaterThanOrEqual(2f).test(2f));
        assertFalse(greaterThanOrEqual(3f).test(2f));
    }

    @Test
    public void testComparableFloatLessThanOrEqual() {
        assertTrue(lessThanOrEqual(5f).test(5f));
        assertFalse(lessThanOrEqual(4f).test(5f));
    }

    @Test
    public void testComparableFloatBetween() {
        assertTrue(between(0f, 5f).test(2f));
        assertFalse(between(5f, 0f).test(6f));
    }

    @Test
    public void testComparableBigIntegerGreaterThan() {
        assertTrue(greaterThan(BigInteger.valueOf(1)).test(BigInteger.valueOf(2)));
        assertFalse(greaterThan(BigInteger.valueOf(2)).test(BigInteger.valueOf(2)));
    }

    @Test
    public void testComparableBigIntegerLessThan() {
        assertTrue(lessThan(BigInteger.valueOf(6)).test(BigInteger.valueOf(5)));
        assertFalse(lessThan(BigInteger.valueOf(5)).test(BigInteger.valueOf(5)));
    }

    @Test
    public void testComparableBigIntegerGreaterThanOrEqual() {
        assertTrue(greaterThanOrEqual(BigInteger.valueOf(2)).test(BigInteger.valueOf(2)));
        assertFalse(greaterThanOrEqual(BigInteger.valueOf(3)).test(BigInteger.valueOf(2)));
    }

    @Test
    public void testComparableBigIntegerLessThanOrEqual() {
        assertTrue(lessThanOrEqual(BigInteger.valueOf(5)).test(BigInteger.valueOf(5)));
        assertFalse(lessThanOrEqual(BigInteger.valueOf(4)).test(BigInteger.valueOf(5)));
    }

    @Test
    public void testComparableBigIntegerBetween() {
        assertTrue(between(BigInteger.valueOf(0), BigInteger.valueOf(5)).test(BigInteger.valueOf(2)));
        assertFalse(between(BigInteger.valueOf(5), BigInteger.valueOf(0)).test(BigInteger.valueOf(6)));
    }

    @Test
    public void testComparableBigDecimalGreaterThan() {
        assertTrue(greaterThan(BigDecimal.valueOf(1)).test(BigDecimal.valueOf(2)));
        assertFalse(greaterThan(BigDecimal.valueOf(2)).test(BigDecimal.valueOf(2)));
    }

    @Test
    public void testComparableBigDecimalLessThan() {
        assertTrue(lessThan(BigDecimal.valueOf(6)).test(BigDecimal.valueOf(5)));
        assertFalse(lessThan(BigDecimal.valueOf(5)).test(BigDecimal.valueOf(5)));
    }

    @Test
    public void testComparableBigDecimalGreaterThanOrEqual() {
        assertTrue(greaterThanOrEqual(BigDecimal.valueOf(2)).test(BigDecimal.valueOf(2)));
        assertFalse(greaterThanOrEqual(BigDecimal.valueOf(3)).test(BigDecimal.valueOf(2)));
    }

    @Test
    public void testComparableBigDecimalLessThanOrEqual() {
        assertTrue(lessThanOrEqual(BigDecimal.valueOf(5)).test(BigDecimal.valueOf(5)));
        assertFalse(lessThanOrEqual(BigDecimal.valueOf(4)).test(BigDecimal.valueOf(5)));
    }

    @Test
    public void testComparableBigDecimalBetween() {
        assertTrue(between(BigDecimal.valueOf(0), BigDecimal.valueOf(5)).test(BigDecimal.valueOf(2)));
        assertFalse(between(BigDecimal.valueOf(5), BigDecimal.valueOf(0)).test(BigDecimal.valueOf(6)));
    }

    @Test
    public void testObjectComparableGreaterThan() {
        assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 1)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 2)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 3)));
    }

    @Test
    public void testNullObjectComparableGreaterThan() {
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(null));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, null)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, 2)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, null)));
    }

    @Test
    public void testObjectComparableGreaterThanOrEqual() {
        assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 1)));
        assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 2)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 3)));
    }

    @Test
    public void testNullObjectComparableGreaterThanOrEqual() {
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(null));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, null)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, 2)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(greaterThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, null)));
    }

    @Test
    public void testObjectComparableLessThan() {
        assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 2)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 1)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 0)));
    }

    @Test
    public void testNullObjectComparableLessThan() {
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(null));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, null)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, 1)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThan(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, null)));
    }

    @Test
    public void testObjectComparableLessThanOrEqual() {
        assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 2)));
        assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 1)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 0)));
    }

    @Test
    public void testNullObjectComparableLessThanOrEqual() {
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(null));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, null)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, 1)));
        assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(lessThanOrEqual(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, null)));
    }

}
