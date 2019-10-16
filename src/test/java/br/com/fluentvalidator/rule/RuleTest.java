package br.com.fluentvalidator.rule;

import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class RuleTest {

  @Test
  public void testSuccessDefaultApply() {

    final Rule<String> rule = new Rule<String>() {};

    assertTrue(rule.apply(new String()));

  }

  @Test
  public void testSuccessDefaultApply2() {

    final Rule<String> rule = new Rule<String>() {};

    assertTrue(rule.apply(new Object(), new String()));

  }

  @Test
  public void testSuccessDefaultSupports() {

    final Rule<String> rule = new Rule<String>() {};

    assertTrue(rule.support(new String()));

  }

}
