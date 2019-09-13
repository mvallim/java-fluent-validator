package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

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
		
		final StringValidationRule rule = new StringValidationRule();
		rule.must(equalTo("o"));
		
		assertTrue(RuleProcessor.process("o", rule));
	}
	
	@Test
	public void testSuccessSingleRuleWithCritical() {
		
		final StringValidationRule rule = new StringValidationRule();
		rule.must(equalTo("o"));
		rule.critical();
		
		assertFalse(RuleProcessor.process("oo", rule));
	}
	
	@Test
	public void testSuccessMultipleRules() {
		
		final Collection<Rule<String>> rules = new LinkedList<>();
		
		rules.add(new StringValidationRule());
		rules.add(new StringValidationRule());
		rules.add(new StringValidationRule());
		
		assertTrue(RuleProcessor.process("o", rules));
	}

	@Test
	public void testFailMultipleRulesWithCritical() {
		
		final Collection<Rule<String>> rules = new LinkedList<>();
		
		final StringValidationRule stringValidationRule = new StringValidationRule();
		stringValidationRule.must(equalTo("o"));
		stringValidationRule.critical();
		
		rules.add(new StringValidationRule());
		rules.add(stringValidationRule);
		rules.add(new StringValidationRule());
		
		assertFalse(RuleProcessor.process("oo", rules));
	}

	@Test
	public void testSuccessSinleRulesAndMultipleValues() {
		
		final StringValidationRule rule = new StringValidationRule();
		rule.must(equalTo("o"));
		
		final Collection<String> values = Arrays.asList("o", "oo");
				
		assertTrue(RuleProcessor.process(values, rule));
	}

	class StringValidationRule extends AbstractValidationRule<String, String> {

		public StringValidationRule() {
			super();
		}

		@Override
		public boolean apply(final String instance) {
			final boolean apply = this.getMust().test(instance);
			return !(Boolean.TRUE.equals(this.isCritical()) && Boolean.FALSE.equals(apply));
		}

		@Override
		public boolean support(final String instance) {
			return Boolean.TRUE.equals(this.getWhen().test(instance));
		}
		
	}
}
