package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.ObjectPredicate.equalObject;
import static br.com.fluentvalidator.predicate.ObjectPredicate.instanceOf;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.function.Function;

import org.junit.Test;

public class ObjectPredicateTest {

  @Test
  public void testNullObjectEqualTo() {
    assertFalse(equalObject("1").test(null));
  }

  @Test
  public void testNullObjectInstanceOf() {
    assertFalse(instanceOf(String.class).test(null));
    assertFalse(instanceOf(null).test(null));
    assertFalse(instanceOf(null).test("he"));
  }

  @Test
  public void testEqualTo() {
    assertTrue(equalObject("1").test("1"));
    assertFalse(equalObject("1").test("he"));
  }

  @Test
  public void testInstanceOf() {
    assertTrue(instanceOf(String.class).test("1"));
    assertTrue(instanceOf(Object.class).test("1"));
    assertTrue(instanceOf(Object.class).test(1));
    assertFalse(instanceOf(String.class).test(1));
    assertFalse(instanceOf(String.class).test(1));
  }

  @Test
  public void testNullValue() {
    assertTrue(nullValue().test(null));
    assertFalse(nullValue().test("false"));
  }

  @Test
  public void testObjectNullValue() {
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(null)).test(null));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(null)).test(new ObjectFrom<>(null, null)));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(fn -> null)).test(null));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(fn -> null)).test(new ObjectFrom<>(null, null)));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue((Function<ObjectFrom<Integer>, Integer>) null)).test(null));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue((Function<ObjectFrom<Integer>, Integer>) null)).test(new ObjectFrom<>(null, null)));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(ObjectFrom::getSource)).test(new ObjectFrom<>(null, null)));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(ObjectFrom::getSource)).test(null));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(nullValue(ObjectFrom::getSource)).test(new ObjectFrom<>(1, null)));
  }

  @Test
  public void testObjectEqualTo() {
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 1)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(2, 1)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, 2)));
  }

  @Test
  public void testObjectEqualTo2() {
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, 1)).test(new ObjectFrom<>(1, null)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, 1)).test(new ObjectFrom<>(2, null)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, 2)).test(new ObjectFrom<>(1, null)));
  }

  @Test
  public void testObjectNullEqualTo() {
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(1, null)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, 1)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, ObjectFrom::getTarget)).test(new ObjectFrom<>(null, null)));
  }

  @Test
  public void testObjectNullEqualTo2() {
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, null)).test(new ObjectFrom<>(1, null)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, null)).test(new ObjectFrom<>(null, 1)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(equalObject(ObjectFrom::getSource, null)).test(new ObjectFrom<>(null, null)));
  }

  @Test
  public void testObjectInstanceOf() {
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(instanceOf(ObjectFrom::getSource, Integer.class)).test(new ObjectFrom<>(1, 1)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(instanceOf(ObjectFrom::getSource, String.class)).test(new ObjectFrom<>(1, 1)));
    assertTrue(PredicateBuilder.<ObjectFrom<Integer>>from(instanceOf(ObjectFrom::getSource, Object.class)).test(new ObjectFrom<>(1, 1)));
  }

  @Test
  public void testObjectNullInstanceOf() {
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(instanceOf(ObjectFrom::getSource, null)).test(new ObjectFrom<>(1, 1)));
    assertFalse(PredicateBuilder.<ObjectFrom<Integer>>from(instanceOf(ObjectFrom::getSource, null)).test(new ObjectFrom<>(null, 1)));
  }

}
