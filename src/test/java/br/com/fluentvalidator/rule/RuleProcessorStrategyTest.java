package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.equalObject;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.junit.Test;

import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationResult;

public class RuleProcessorStrategyTest {

  @Test
  public void testDefaultSuccessSingleRule() {

    final StringValidationRule rule = new StringValidationRule();
    rule.must(equalObject("o"));

    assertTrue(RuleProcessorStrategy.getDefault().process("o", rule));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertTrue(validationResult.isValid());
  }

  @Test
  public void testDefaultSuccessSingleRuleWithCritical() {

    final StringValidationRule rule = new StringValidationRule();
    rule.must(equalObject("o"));
    rule.critical();

    assertFalse(RuleProcessorStrategy.getDefault().process("oo", rule));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertFalse(validationResult.isValid());

    assertThat(validationResult.getErrors(), not(empty()));
    assertThat(validationResult.getErrors(), hasSize(1));
  }

  @Test
  public void testDefaultSuccessMultipleRules() {

    final Collection<Rule<String>> rules = new LinkedList<>();

    rules.add(new StringValidationRule());
    rules.add(new StringValidationRule());
    rules.add(new StringValidationRule());

    assertTrue(RuleProcessorStrategy.getDefault().process("o", rules));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertTrue(validationResult.isValid());
  }

  @Test
  public void testDefaultFailMultipleRulesWithCritical() {

    final Collection<Rule<String>> rules = new LinkedList<>();

    final StringValidationRule stringValidationRule = new StringValidationRule();
    stringValidationRule.must(equalObject("o"));
    stringValidationRule.critical();

    rules.add(new StringValidationRule());
    rules.add(stringValidationRule);
    rules.add(new StringValidationRule());

    assertFalse(RuleProcessorStrategy.getDefault().process("oo", rules));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertFalse(validationResult.isValid());

    assertThat(validationResult.getErrors(), not(empty()));
    assertThat(validationResult.getErrors(), hasSize(1));
  }

  @Test
  public void testDefaultSuccessSinleRulesAndMultipleValues() {

    final StringValidationRule rule = new StringValidationRule();
    rule.must(equalObject("o"));

    final Collection<String> values = Arrays.asList("o", "oo");

    assertTrue(RuleProcessorStrategy.getDefault().process(values, rule));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertFalse(validationResult.isValid());

    assertThat(validationResult.getErrors(), not(empty()));
    assertThat(validationResult.getErrors(), hasSize(1));
  }

  @Test
  public void testDefaultSuccessCritical() {

    final StringValidationRule rule = new StringValidationRule();
    rule.must(not(nullValue()));
    rule.critical();

    assertFalse(RuleProcessorStrategy.getDefault().process((String) null, rule));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertFalse(validationResult.isValid());

    assertThat(validationResult.getErrors(), not(empty()));
    assertThat(validationResult.getErrors(), hasSize(1));

    assertTrue(RuleProcessorStrategy.getDefault().process("o", rule));
  }

  @Test
  public void testFailFastFailMultipleRulesWithCritical() {

    final Collection<Rule<String>> rules = new LinkedList<>();

    final StringValidationRule stringValidationRule1 = new StringValidationRule();
    stringValidationRule1.must(equalObject("o"));
    stringValidationRule1.withMessage(obj -> "Rule non critical 1");

    final StringValidationRule stringValidationRuleCritical = new StringValidationRule();
    stringValidationRuleCritical.must(equalObject("o"));
    stringValidationRuleCritical.withMessage(obj -> "Rule critical 1");
    stringValidationRuleCritical.critical();

    final StringValidationRule stringValidationRule2 = new StringValidationRule();
    stringValidationRule2.must(equalObject("o"));
    stringValidationRule2.withMessage(obj -> "Rule non critical 2");

    rules.add(stringValidationRule1);
    rules.add(stringValidationRuleCritical);
    rules.add(stringValidationRule2);

    assertFalse(RuleProcessorStrategy.getFailFast().process("oo", rules));

    final ValidationResult validationResult = ValidationContext.get().getValidationResult();

    assertFalse(validationResult.isValid());

    assertThat(validationResult.getErrors(), not(empty()));
    assertThat(validationResult.getErrors(), hasSize(2));

    assertThat(validationResult.getErrors(), hasItem(hasProperty("message", containsString("Rule non critical 1"))));
    assertThat(validationResult.getErrors(), hasItem(hasProperty("message", containsString("Rule critical 1"))));
    assertThat(validationResult.getErrors(), hasItem(hasProperty("message", not(containsString("Rule critical 2")))));
  }

  class StringValidationRule extends AbstractValidationRule<String, String> {

    public StringValidationRule() {
      super();
    }

    @Override
    public boolean apply(final String instance) {
      final boolean apply = getMust().test(instance);

      if (Boolean.FALSE.equals(apply)) {
        ValidationContext.get().addErrors(getHandlerInvalid().handle(instance, instance));
      }

      return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
    }

    @Override
    public boolean support(final String instance) {
      return Boolean.TRUE.equals(getWhen().test(instance));
    }

  }
}
