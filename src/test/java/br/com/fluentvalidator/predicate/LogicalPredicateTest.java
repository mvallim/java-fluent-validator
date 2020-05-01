package br.com.fluentvalidator.predicate;

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
    assertTrue(isTrue().test(true));
    assertTrue(isFalse().test(false));
    assertTrue(isTrue().test(Boolean.TRUE));
    assertTrue(isFalse().test(Boolean.FALSE));
    assertFalse(isTrue().test(null));
    assertFalse(isFalse().test(null));
  }

  @Test
  public void testLogicalPredicatesIsTrue() {
    assertTrue(isTrue(fn -> true).test(new String()));
    assertFalse(isTrue(fn -> false).test(new String()));
    assertFalse(isTrue(fn -> false).test(null));
  }

  @Test
  public void testLogicalPredicatesIsFalse() {
    assertTrue(isFalse(fn -> false).test(new String()));
    assertFalse(isFalse(fn -> true).test(new String()));
    assertFalse(isFalse(fn -> true).test(null));
  }

}
