package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ObjectPredicate.equalTo;
import static br.com.fluentvalidator.predicate.ObjectPredicate.instanceOf;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ObjectPredicateTest {

  @Test
  public void testNullObjectEqualTo() {
    assertFalse(equalTo("1").test(null));
  }

  @Test
  public void testNullObjectInstanceOf() {
    assertFalse(instanceOf(String.class).test(null));
  }

  @Test
  public void testEqualTo() {
    assertTrue(equalTo("1").test("1"));
    assertFalse(equalTo("1").test("he"));
  }

  @Test
  public void testInstanceOf() {
    assertTrue(instanceOf(String.class).test("1"));
    assertFalse(instanceOf(String.class).test(1));
  }

  @Test
  public void testNullValue() {
    assertTrue(nullValue().test(null));
    assertFalse(nullValue().test("false"));
  }

}
