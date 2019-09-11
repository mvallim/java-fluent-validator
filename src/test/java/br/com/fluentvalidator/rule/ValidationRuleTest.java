package br.com.fluentvalidator.rule;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static br.com.fluentvalidator.predicate.ObjectPredicate.*;
import static br.com.fluentvalidator.predicate.LogicalPredicate.*;

import java.util.function.Predicate;

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
		
		final StringValidationRule rule = new StringValidationRule(not(nullValue()));
		
		assertFalse(rule.getWhen().test(null));
		assertTrue(rule.getWhen().test("o"));
	}

	@Test
	public void testSuccessMust() {
		
		final StringValidationRule rule = new StringValidationRule(isTrue());
		rule.must(not(nullValue()));
		
		assertFalse(rule.getMust().test(null));
		assertTrue(rule.getMust().test("o"));
	}

	@Test
	public void testSuccessApply() {
		
		final StringValidationRule rule = new StringValidationRule(isTrue());
		rule.must(not(nullValue()));
		
		assertTrue(rule.apply(null));
		assertTrue(rule.apply("o"));
	}

	class StringValidationRule extends AbstractRuleDescriptor<String, String> {

		protected StringValidationRule(Predicate<String> when) {
			super(when);
		}

		@Override
		boolean accept(String instance) {
			return false;
		}
		
	}

}
