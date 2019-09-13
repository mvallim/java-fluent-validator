package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isFalse;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.Test;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.exception.ValidationSampleException;

public class RuleBuilderCollectionTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}
	
	@Test
	public void testFailWhenDontApplyNullValue() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertFalse(builder.apply(null));
	}
	
	@Test
	public void testSuccessValidValue() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testSuccessInvalidSingleRuleWithoutCritical() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(1))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}
	
	@Test
	public void testSuccessInvalidMultipleRuleWithoutCritical() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(hasSize(1))
				.when(not(nullValue()))
				.withMessage("test")
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testFailInvalidSingleRuleWithCritical() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder.must(hasSize(1))
			.when(not(nullValue()))		
			.withMessage("test")
			.critical();
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testFailInvalidMultipleRuleWithCritical() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(hasSize(1))
				.when(not(nullValue()))
				.withMessage("test")
				.critical()
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	
	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidSingleWithCriticalException() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder.must(hasSize(1))
			.when(not(nullValue()))		
			.withMessage("test")
			.critical(ValidationSampleException.class);
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidMultipleWithCriticalException() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(hasSize(2))
				.when(not(nullValue()))	
				.withMessage("test")
			.must(hasSize(1))
				.when(not(nullValue()))
				.withMessage("test")
				.critical(ValidationSampleException.class)
			.must(hasSize(2))
				.when(not(nullValue()))			
				.withMessage("test");
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testSuccessValidAndInvalidMultipleRule() {
		
		final RuleBuilderCollection<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(isFalse())
				.when(isTrue())
				.withMessage("ever enter here")
				.withCode("666")
				.withFieldName("size")
			.must(isTrue())
				.when(isTrue())
				.withMessage("never enter here")
				.withCode("666")
				.withFieldName("size")
			.must(isTrue())
				.when(isFalse())
				.withMessage("never enter here")
				.withCode("666")
				.withFieldName("size")
			.must(isFalse())
				.when(isFalse())
				.withMessage("never enter here")
				.withCode("666")
				.withFieldName("size");
		
		assertTrue(builder.apply(Arrays.asList("o")));
	}
}