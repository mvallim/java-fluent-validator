package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.CollectionPredicate.empty;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasAny;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasItem;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasItems;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;

public class CollectionPredicateTest {

    @Test
    public void testNullCollectionEmpty() {
        assertTrue(empty().test(null));
    }

    @Test
    public void testNullCollectionHasItems() {
        assertFalse(hasItems(new Integer[] { 1 }).test(null));
        assertFalse(hasItems((Integer[]) null).test(null));
        assertFalse(hasItems((Integer[]) null).test(Arrays.asList(1)));
        assertFalse(hasItems((Collection<Integer>) null).test(null));
        assertFalse(hasItems((Collection<Integer>) null).test(Arrays.asList(1)));
    }

    @Test
    public void testNullCollectionHasItem() {
        assertFalse(hasItem(1).test(null));
        assertFalse(hasItem((Integer) null).test(null));
        assertFalse(hasItem((Integer) null).test(Arrays.asList(1)));
    }

    @Test
    public void testNullCollectionHasAny() {
        assertFalse(hasAny(new Integer[] { 1 }).test(null));
        assertFalse(hasAny((Integer[]) null).test(null));
        assertFalse(hasAny((Integer[]) null).test(Arrays.asList(1)));
        assertFalse(hasAny((Collection<Integer>) null).test(Arrays.asList(1)));
        assertFalse(hasAny((Collection<Integer>) null).test(null));
    }

    @Test
    public void testNullCollectionHasSize() {
        assertFalse(hasSize(1).test(null));
        assertFalse(hasSize(null).test(null));
        assertFalse(hasSize(null).test(Arrays.asList(1)));
    }

    @Test
    public void testCollectionEmty() {
        final String element = "1";
        assertTrue(empty().test(Arrays.asList()));
        assertFalse(empty().test(Arrays.asList(element)));
    }

    @Test
    public void testCollectionHasItems() {
        final String element = "1";
        assertTrue(hasItems(new String[] { element }).test(Arrays.asList(element)));
        assertFalse(hasItems(new String[] { "1" }).test(Arrays.asList()));
    }

    @Test
    public void testCollectionHasItem() {
        final String element = "1";
        assertTrue(hasItem(element).test(Arrays.asList(element)));
        assertFalse(hasItem("1").test(Arrays.asList()));
    }

    @Test
    public void testCollectionHasAny() {
        final String element = "1";
        assertTrue(hasAny(new String[] { element }).test(Arrays.asList(element)));
        assertFalse(hasAny(new String[] { "1" }).test(Arrays.asList()));
    }

    @Test
    public void testCollectionHasSize() {
        final String element = "1";
        assertTrue(hasSize(1).test(Arrays.asList(element)));
        assertFalse(hasSize(1).test(Arrays.asList()));
    }

    @Test
    public void testObjectNullCollectionEmpty() {
        assertTrue(empty(TestClass::getSource).test(new TestClass(null)));
    }

    @Test
    public void testObjectNullCollectionHasItems() {
        assertFalse(hasItems(TestClass::getSource, new Integer[] { 1 }).test(null));
        assertFalse(hasItems(TestClass::getSource, (Integer[]) null).test(null));
        assertFalse(hasItems(TestClass::getSource, (Integer[]) null).test(new TestClass(Arrays.asList(1))));
        assertFalse(hasItems(TestClass::getSource, (Collection<Integer>) null).test(null));
        assertFalse(hasItems(TestClass::getSource, (Collection<Integer>) null).test(new TestClass(Arrays.asList(1))));
    }

    @Test
    public void testObjectNullCollectionHasItem() {
        assertFalse(hasItem(TestClass::getSource, 1).test(null));
        assertFalse(hasItem(TestClass::getSource, (Integer) null).test(null));
        assertFalse(hasItem(TestClass::getSource, (Integer) null).test(new TestClass(Arrays.asList(1))));
    }

    @Test
    public void testObjectNullCollectionHasAny() {
        assertFalse(hasAny(TestClass::getSource, new Integer[] { 1 }).test(null));
        assertFalse(hasAny(TestClass::getSource, (Integer[]) null).test(null));
        assertFalse(hasAny(TestClass::getSource, (Integer[]) null).test(new TestClass(Arrays.asList(1))));
        assertFalse(hasAny(TestClass::getSource, (Collection<Integer>) null).test(new TestClass(Arrays.asList(1))));
        assertFalse(hasAny(TestClass::getSource, (Collection<Integer>) null).test(null));
    }

    @Test
    public void testObjectNullCollectionHasSize() {
        assertFalse(hasSize(TestClass::getSource, 1).test(null));
        assertFalse(hasSize(TestClass::getSource, null).test(null));
        assertFalse(hasSize(TestClass::getSource, null).test(new TestClass(Arrays.asList(1))));
    }

    @Test
    public void testObjectCollectionEmty() {
        assertTrue(empty(TestClass::getSource).test(new TestClass(Arrays.asList())));
        assertFalse(empty(TestClass::getSource).test(new TestClass(Arrays.asList(1))));
    }

    @Test
    public void testObjectCollectionHasItems() {
        final Integer element = 1;
        assertTrue(hasItems(TestClass::getSource, new Integer[] { element }).test(new TestClass(Arrays.asList(element))));
        assertFalse(hasItems(TestClass::getSource, new Integer[] { 1 }).test(new TestClass(Arrays.asList())));
    }

    @Test
    public void testObjectCollectionHasItem() {
        final Integer element = 1;
        assertTrue(hasItem(TestClass::getSource, element).test(new TestClass(Arrays.asList(element))));
        assertFalse(hasItem(TestClass::getSource, 1).test(new TestClass(Arrays.asList())));
    }

    @Test
    public void testObjectCollectionHasAny() {
        final Integer element = 1;
        assertTrue(hasAny(TestClass::getSource, new Integer[] { element }).test(new TestClass(Arrays.asList(element))));
        assertFalse(hasAny(TestClass::getSource, new Integer[] { 1 }).test(new TestClass(Arrays.asList())));
    }

    @Test
    public void testObjectCollectionHasSize() {
        assertTrue(hasSize(TestClass::getSource, 1).test(new TestClass(Arrays.asList(1))));
        assertFalse(hasSize(TestClass::getSource, 1).test(new TestClass(Arrays.asList())));
    }

    class TestClass {

        private final Collection<Integer> source;

        private TestClass(final Collection<Integer> source) {
            this.source = source;
        }

        public Collection<Integer> getSource() {
            return source;
        }

    }
}
