package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isFalse;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Test;

import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.exception.ValidationSampleException;

public class PropertyRuleBuilderTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}

	@Test
	public void testFailWhenDontApplyNullValue() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder.when(not(nullValue())).must(lessThan(2)).withMessage("test");
		
		assertFalse(builder.apply(null));
	}
	
	@Test
	public void testSuccessValidValue() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder.when(not(nullValue())).must(lessThan(2)).withMessage("test");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessInvalidSingleRuleWithoutCritical() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder.when(not(nullValue())).must(lessThan(1)).withMessage("test");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessInvalidMultipleRuleWithoutCritical() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder
			.when(not(nullValue())).must(lessThan(2)).withMessage("test")
			.when(not(nullValue())).must(lessThan(2)).withMessage("test")
			.when(not(nullValue())).must(lessThan(1)).withMessage("test")
			.when(not(nullValue())).must(lessThan(2)).withMessage("test");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testFailInvalidSingleRuleWithCritical() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder.when(not(nullValue())).must(lessThan(1)).withMessage("test").critical();
		
		assertFalse(builder.apply("o"));
	}

	@Test
	public void testFailInvalidMultipleRuleWithCritical() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);

		builder
			.when(not(nullValue())).must(lessThan(2)).withMessage("test")
			.when(not(nullValue())).must(lessThan(2)).withMessage("test")
			.when(not(nullValue())).must(lessThan(1)).withMessage("test").critical()
			.when(not(nullValue())).must(lessThan(2)).withMessage("test");

		assertFalse(builder.apply("o"));
	}


	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidSingleRuleWithCriticalException() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder.when(not(nullValue())).must(lessThan(1)).withMessage("test").critical(ValidationSampleException.class);
		
		assertFalse(builder.apply("o"));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidMultipleRuleWithCriticalException() {
		
		final PropertyRuleBuilder<String, Integer> builder = new PropertyRuleBuilder<>(String::length);
		
		builder
			.when(not(nullValue())).must(lessThan(2)).withMessage("test")
			.when(not(nullValue())).must(lessThan(2)).withMessage("test")
			.when(not(nullValue())).must(lessThan(1)).withMessage("test").critical(ValidationSampleException.class)
			.when(not(nullValue())).must(lessThan(2)).withMessage("test");
		
		assertFalse(builder.apply("o"));
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
