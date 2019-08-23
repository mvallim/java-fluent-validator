package br.com.fluentvalidator.predicate.test;

import static br.com.fluentvalidator.predicate.LogicalPredicate.isFalse;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class LogicalPredicateTest {

	@Test
	public void testLogicalPredicates() {
		assertTrue(not(Integer.class::isInstance).test(new String()));
		assertTrue(isTrue().test(null));
		assertFalse(isFalse().test(null));
	}

}
