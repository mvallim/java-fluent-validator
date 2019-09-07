package br.com.fluentvalidator.predicate.test;

import static br.com.fluentvalidator.predicate.CollectionPredicate.empty;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasAny;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasItem;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasItems;
import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

public class CollectionPredicateTest {

	@Test
	public void testNullCollectionEmpty() {
		assertFalse(empty().test(null));
	}

	@Test
	public void testNullCollectionHasItems() {
		assertFalse(hasItems(1).test(null));
	}

	@Test
	public void testNullCollectionHasItem() {
		assertFalse(hasItem(1).test(null));
	}

	@Test
	public void testNullCollectionHasAny() {
		assertFalse(hasAny(1).test(null));
	}

	@Test
	public void testNullCollectionHasSize() {
		assertFalse(hasSize(1).test(null));
	}

	@Test
	public void testCollectionPredicates() {
		final String element = "1";

		assertTrue(empty().test(Arrays.asList()));
		assertFalse(empty().test(Arrays.asList(element)));

		assertTrue(hasItems(element).test(Arrays.asList(element)));
		assertFalse(hasItems("1").test(Arrays.asList()));

		assertTrue(hasItem(element).test(Arrays.asList(element)));
		assertFalse(hasItem("1").test(Arrays.asList()));

		assertTrue(hasAny(element).test(Arrays.asList(element)));
		assertFalse(hasAny("1").test(Arrays.asList()));

		assertTrue(hasSize(1).test(Arrays.asList(element)));
		assertFalse(hasSize(1).test(Arrays.asList()));
	}

}
