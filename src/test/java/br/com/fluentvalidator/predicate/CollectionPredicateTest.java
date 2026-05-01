/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.CollectionPredicate.empty;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasAny;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasItem;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasItems;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSizeBetween;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSizeBetweenInclusive;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.api.Test;

class CollectionPredicateTest {

  @Test
  void testNullCollectionEmpty() {
    assertTrue(empty().test(null));
  }

  @Test
  void testNullCollectionHasItems() {
    assertFalse(hasItems(new Integer[] { 1 }).test(null));
    assertFalse(hasItems((Integer[]) null).test(null));
    assertFalse(hasItems((Integer[]) null).test(Arrays.asList(1)));
    assertFalse(hasItems((Collection<Integer>) null).test(null));
    assertFalse(hasItems((Collection<Integer>) null).test(Arrays.asList(1)));
  }

  @Test
  void testNullCollectionHasItem() {
    assertFalse(hasItem(1).test(null));
    assertFalse(hasItem((Integer) null).test(null));
    assertFalse(hasItem((Integer) null).test(Arrays.asList(1)));
  }

  @Test
  void testNullCollectionHasAny() {
    assertFalse(hasAny(new Integer[] { 1 }).test(null));
    assertFalse(hasAny((Integer[]) null).test(null));
    assertFalse(hasAny((Integer[]) null).test(Arrays.asList(1)));
    assertFalse(hasAny((Collection<Integer>) null).test(Arrays.asList(1)));
    assertFalse(hasAny((Collection<Integer>) null).test(null));
  }

  @Test
  void testNullCollectionHasSize() {
    assertFalse(hasSize(1).test(null));
    assertFalse(hasSize(null).test(null));
    assertFalse(hasSize(null).test(Arrays.asList(1)));
  }

  @Test
  void testCollectionEmty() {
    final String element = "1";
    assertTrue(empty().test(Arrays.asList()));
    assertFalse(empty().test(Arrays.asList(element)));
  }

  @Test
  void testCollectionHasItems() {
    final String element = "1";
    assertTrue(hasItems(new String[] { element }).test(Arrays.asList(element)));
    assertFalse(hasItems(new String[] { "1" }).test(Arrays.asList()));
  }

  @Test
  void testCollectionHasItem() {
    final String element = "1";
    assertTrue(hasItem(element).test(Arrays.asList(element)));
    assertFalse(hasItem("1").test(Arrays.asList()));
  }

  @Test
  void testCollectionHasAny() {
    final String element = "1";
    assertTrue(hasAny(new String[] { element }).test(Arrays.asList(element)));
    assertFalse(hasAny(new String[] { "1" }).test(Arrays.asList()));
  }

  @Test
  void testCollectionHasSize() {
    final String element = "1";
    assertTrue(hasSize(1).test(Arrays.asList(element)));
    assertFalse(hasSize(1).test(Arrays.asList()));
  }

  @Test
  void testCollectionHasSizeBetween() {
    final List<Integer> asList = Arrays.asList(1, 2, 3, 4);
    assertTrue(hasSizeBetween(fn -> asList, 2, 5).test(asList));
    assertFalse(hasSizeBetween(fn -> asList, 1, 2).test(asList));
  }

  @Test
  void testCollectionHasSizeBetween01() {
    assertTrue(hasSizeBetween(2, 5).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetween(1, 2).test(Arrays.asList(1, 2, 3, 4)));
  }

  @Test
  void testCollectionHasSizeBetweenInclusive() {
    final List<Integer> asList = Arrays.asList(1, 2, 3, 4);
    assertTrue(hasSizeBetweenInclusive(fn -> asList, 1, 4).test(asList));
    assertTrue(hasSizeBetweenInclusive(fn -> asList, 1, 10).test(asList));
    assertTrue(hasSizeBetweenInclusive(fn -> asList, 4, 4).test(asList));
    assertFalse(hasSizeBetweenInclusive(fn -> asList, 4, 1).test(asList));
    assertFalse(hasSizeBetweenInclusive(fn -> asList, 1, 2).test(asList));
  }

  @Test
  void testCollectionHasSizeBetweenInclusive01() {
    assertTrue(hasSizeBetweenInclusive(1, 4).test(Arrays.asList(1, 2, 3, 4)));
    assertTrue(hasSizeBetweenInclusive(1, 10).test(Arrays.asList(1, 2, 3, 4)));
    assertTrue(hasSizeBetweenInclusive(4, 4).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetweenInclusive(4, 1).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetweenInclusive(1, 2).test(Arrays.asList(1, 2, 3, 4)));
  }

  @Test
  void testCollectionHasSizeBetweenNull() {
    assertFalse(hasSizeBetween(null, null).test(null));
    assertFalse(hasSizeBetween(1, null).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetween(null, 2).test(null));
    assertFalse(hasSizeBetween(1, 2).test(null));
  }

  @Test
  void testCollectionHasSizeBetweenNull01() {
    final List<Integer> asList = Arrays.asList(1, 2, 3, 4);
    assertFalse(hasSizeBetween(null, null, null).test(null));
    assertFalse(hasSizeBetween(fn -> asList, null, 2).test(asList));
    assertFalse(hasSizeBetween(fn -> asList, 1, null).test(null));
    assertFalse(hasSizeBetween(fn -> asList, 1, 2).test(null));
  }

  @Test
  void testCollectionHasSizeBetweenInclusiveNull01() {
    final List<Integer> asList = Arrays.asList(1, 2, 3, 4);
    assertFalse(hasSizeBetweenInclusive(fn -> asList, null, null).test(null));
    assertFalse(hasSizeBetweenInclusive(fn -> asList, 1, null).test(asList));
    assertFalse(hasSizeBetweenInclusive(fn -> asList, null, 4).test(null));
    assertFalse(hasSizeBetweenInclusive(fn -> asList, 1, 2).test(null));
  }

  @Test
  void testCollectionHasSizeBetweenInclusiveNull() {
    assertFalse(hasSizeBetweenInclusive(null, null).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetweenInclusive(1, null).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetweenInclusive(null, 4).test(Arrays.asList(1, 2, 3, 4)));
    assertFalse(hasSizeBetweenInclusive(4, 1).test(null));
  }

  @Test
  void testObjectNullCollectionEmpty() {
    assertTrue(empty(TestClass::getSource).test(new TestClass(null)));
  }

  @Test
  void testObjectNullCollectionHasItems() {
    assertFalse(hasItems(TestClass::getSource, new Integer[] { 1 }).test(null));
    assertFalse(hasItems(TestClass::getSource, (Integer[]) null).test(null));
    assertFalse(hasItems(TestClass::getSource, (Integer[]) null).test(new TestClass(Arrays.asList(1))));
    assertFalse(hasItems(TestClass::getSource, (Collection<Integer>) null).test(null));
    assertFalse(hasItems(TestClass::getSource, (Collection<Integer>) null).test(new TestClass(Arrays.asList(1))));
  }

  @Test
  void testObjectNullCollectionHasItem() {
    assertFalse(hasItem(TestClass::getSource, 1).test(null));
    assertFalse(hasItem(TestClass::getSource, (Integer) null).test(null));
    assertFalse(hasItem(TestClass::getSource, (Integer) null).test(new TestClass(Arrays.asList(1))));
  }

  @Test
  void testObjectNullCollectionHasAny() {
    assertFalse(hasAny(TestClass::getSource, new Integer[] { 1 }).test(null));
    assertFalse(hasAny(TestClass::getSource, (Integer[]) null).test(null));
    assertFalse(hasAny(TestClass::getSource, (Integer[]) null).test(new TestClass(Arrays.asList(1))));
    assertFalse(hasAny(TestClass::getSource, (Collection<Integer>) null).test(new TestClass(Arrays.asList(1))));
    assertFalse(hasAny(TestClass::getSource, (Collection<Integer>) null).test(null));
  }

  @Test
  void testObjectNullCollectionHasSize() {
    assertFalse(hasSize(TestClass::getSource, 1).test(null));
    assertFalse(hasSize(TestClass::getSource, (Integer) null).test(null));
    assertFalse(hasSize(TestClass::getSource, (Integer) null).test(new TestClass(Arrays.asList(1))));
    assertFalse(hasSize(TestClass::getSource, (Integer) null).test(new TestClass(null)));
  }

  @Test
  void testObjectNullCollectionHasSize2() {
    assertFalse(hasSize(TestClass::getSource, fn -> 1).test(null));
    assertFalse(hasSize(TestClass::getSource, TestClass::getSize).test(null));
    assertFalse(hasSize(TestClass::getSource, TestClass::getSize).test(new TestClass(Arrays.asList(1))));
    assertFalse(hasSize(TestClass::getSource, TestClass::getSize).test(new TestClass(null)));
  }

  @Test
  void testObjectCollectionEmty() {
    assertTrue(empty(TestClass::getSource).test(new TestClass(Arrays.asList())));
    assertFalse(empty(TestClass::getSource).test(new TestClass(Arrays.asList(1))));
  }

  @Test
  void testObjectCollectionHasItems() {
    final Integer element = 1;
    assertTrue(hasItems(TestClass::getSource, new Integer[] { element }).test(new TestClass(Arrays.asList(element))));
    assertFalse(hasItems(TestClass::getSource, new Integer[] { 1 }).test(new TestClass(Arrays.asList())));
  }

  @Test
  void testObjectCollectionHasItem() {
    final Integer element = 1;
    assertTrue(hasItem(TestClass::getSource, element).test(new TestClass(Arrays.asList(element))));
    assertFalse(hasItem(TestClass::getSource, 1).test(new TestClass(Arrays.asList())));
  }

  @Test
  void testObjectCollectionHasAny() {
    final Integer element = 1;
    assertTrue(hasAny(TestClass::getSource, new Integer[] { element }).test(new TestClass(Arrays.asList(element))));
    assertFalse(hasAny(TestClass::getSource, new Integer[] { 1 }).test(new TestClass(Arrays.asList())));
  }

  @Test
  void testObjectCollectionHasSize() {
    assertTrue(hasSize(TestClass::getSource, 1).test(new TestClass(Arrays.asList(1))));
    assertFalse(hasSize(TestClass::getSource, 1).test(new TestClass(Arrays.asList())));
  }

  @Test
  void testObjectCollectionHasSize2() {
    assertTrue(hasSize(TestClass::getSource, TestClass::getSize).test(new TestClass(Arrays.asList(1), 1)));
    assertFalse(hasSize(TestClass::getSource, TestClass::getSize).test(new TestClass(Arrays.asList(), 1)));
  }

  class TestClass {

    private final Integer size;
    private final Collection<Integer> source;

    private TestClass(final Collection<Integer> source, final Integer size) {
      this.source = source;
      this.size = size;
    }

    private TestClass(final Collection<Integer> source) {
      this(source, null);
    }

    public Collection<Integer> getSource() {
      return source;
    }

    public Integer getSize() {
      return size;
    }

  }
}
