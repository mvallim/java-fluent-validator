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
import br.com.fluentvalidator.exception.ValidationSampleException;

public class CollectionRuleBuilderTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}
	
	@Test
	public void testFailWhenDontApplyNullValue() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder.when(not(nullValue())).must(hasSize(2)).withMessage("test");
		
		assertFalse(builder.apply(null));
	}
	
	@Test
	public void testSuccessValidValue() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder.when(not(nullValue())).must(hasSize(2)).withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testSuccessInvalidSingleRuleWithoutCritical() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder.when(not(nullValue())).must(hasSize(1)).withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}
	
	@Test
	public void testSuccessInvalidMultipleRuleWithoutCritical() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder
			.when(not(nullValue())).must(hasSize(2)).withMessage("test")
			.when(not(nullValue())).must(hasSize(2)).withMessage("test")
			.when(not(nullValue())).must(hasSize(1)).withMessage("test")
			.when(not(nullValue())).must(hasSize(2)).withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testFailInvalidSingleRuleWithCritical() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder.when(not(nullValue())).must(hasSize(1)).withMessage("test").critical();
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testFailInvalidMultipleRuleWithCritical() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder
			.when(not(nullValue())).must(hasSize(2)).withMessage("test")
			.when(not(nullValue())).must(hasSize(2)).withMessage("test")
			.when(not(nullValue())).must(hasSize(1)).withMessage("test").critical()
			.when(not(nullValue())).must(hasSize(2)).withMessage("test");
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	
	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidSingleWithCriticalException() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder.when(not(nullValue())).must(hasSize(1)).withMessage("test").critical(ValidationSampleException.class);
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidMultipleWithCriticalException() {
		
		final CollectionRuleBuilder<List<String>, String> builder = new CollectionRuleBuilder<>(s -> s);
		
		builder
			.when(not(nullValue())).must(hasSize(2)).withMessage("test")
			.when(not(nullValue())).must(hasSize(2)).withMessage("test")
			.when(not(nullValue())).must(hasSize(1)).withMessage("test").critical(ValidationSampleException.class)
			.when(not(nullValue())).must(hasSize(2)).withMessage("test");
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testSuccessValidAndInvalidMultipleRule() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder
			.when(isTrue()).must(isFalse()).withMessage("ever enter here").withCode("666").withFieldName("size")
			.when(isTrue()).must(isTrue()).withMessage("never enter here").withCode("666").withFieldName("size")
			.when(isFalse()).must(isTrue()).withMessage("never enter here").withCode("666").withFieldName("size")
			.when(isFalse()).must(isFalse()).withMessage("never enter here").withCode("666").withFieldName("size");
		
		assertTrue(builder.apply("o"));
	}
}
