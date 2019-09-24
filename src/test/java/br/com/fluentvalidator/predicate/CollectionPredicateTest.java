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
		assertFalse(empty().test(null));
	}

	@Test
	public void testNullCollectionHasItems() {
		assertFalse(hasItems(new Integer[] { 1 }).test(null));
		assertFalse(hasItems((Integer) null).test(null));
		assertFalse(hasItems((Integer) null).test(Arrays.asList(1)));
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
		assertFalse(hasAny((Integer) null).test(null));
		assertFalse(hasAny((Integer) null).test(Arrays.asList(1)));
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

}
