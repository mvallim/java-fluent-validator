package br.com.fluentvalidator.rule;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import static br.com.fluentvalidator.predicate.ObjectPredicate.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.function.Predicate;

import org.junit.After;
import org.junit.Test;

import br.com.fluentvalidator.ValidationContext;

public class RuleProcessorTest {

	@After
	public void tearDown() {
		ValidationContext.remove();
	}

	@Test
	public void testSuccessSingleRule() {
		
		final StringValidationRule rule = new StringValidationRule(when -> true);
		rule.must(equalTo("o"));
		
		assertTrue(RuleProcessor.process("o", rule));
	}
	
	@Test
	public void testSuccessSingleRuleWithCritical() {
		
		final StringValidationRule rule = new StringValidationRule(when -> true);
		rule.must(equalTo("o"));
		rule.critical();
		
		assertFalse(RuleProcessor.process("oo", rule));
	}
	
	@Test
	public void testSuccessMultipleRules() {
		
		final Collection<Rule<String>> rules = new LinkedList<>();
		
		rules.add(new StringValidationRule(when -> true));
		rules.add(new StringValidationRule(when -> true));
		rules.add(new StringValidationRule(when -> true));
		
		assertTrue(RuleProcessor.process("o", rules));
	}

	@Test
	public void testFailMultipleRulesWithCritical() {
		
		final Collection<Rule<String>> rules = new LinkedList<>();
		
		final StringValidationRule stringValidationRule = new StringValidationRule(when -> true);
		stringValidationRule.must(equalTo("o"));
		stringValidationRule.critical();
		
		rules.add(new StringValidationRule(when -> true));
		rules.add(stringValidationRule);
		rules.add(new StringValidationRule(when -> true));
		
		assertFalse(RuleProcessor.process("oo", rules));
	}

	@Test
	public void testSuccessSinleRulesAndMultipleValues() {
		
		final StringValidationRule rule = new StringValidationRule(when -> true);
		rule.must(equalTo("o"));
		
		final Collection<String> values = Arrays.asList("o", "oo");
				
		assertTrue(RuleProcessor.process(values, rule));
	}

	class StringValidationRule extends AbstractRuleDescriptor<String, String> {

		protected StringValidationRule(final Predicate<String> when) {
			super(when);
		}

		@Override
		boolean accept(final String instance) {
			return false;
		}
		
	}
}
