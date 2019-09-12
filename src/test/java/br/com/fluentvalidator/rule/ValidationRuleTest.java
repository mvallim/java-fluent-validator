package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Test;

import br.com.fluentvalidator.ValidationContext;

public class ValidationRuleTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}
	
	@Test
	public void testSuccessWhen() {
		
		final ValidationDescriptorImpl<String> rule = new ValidationDescriptorImpl<>(not(nullValue()));
		
		assertTrue(rule.getWhen().test(null));
		assertTrue(rule.getWhen().test("o"));
	}

	@Test
	public void testSuccessMust() {
		
		final ValidationDescriptorImpl<String> rule = new ValidationDescriptorImpl<>(isTrue());
		rule.must(not(nullValue()));
		
		assertFalse(rule.getMust().test(null));
		assertTrue(rule.getMust().test("o"));
	}

	@Test
	public void testSuccessApply() {
		
		final ValidationDescriptorImpl<String> rule = new ValidationDescriptorImpl<>(isTrue());
		rule.must(not(nullValue()));
		
		assertTrue(rule.apply(null));
		assertTrue(rule.apply("o"));
	}

}
