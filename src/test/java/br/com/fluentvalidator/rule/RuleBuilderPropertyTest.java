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
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.exception.ValidationSampleException;

public class RuleBuilderPropertyTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}

	@Test
	public void testFailWhenDontApplyNullValue() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertFalse(builder.apply(null));
	}
	
	@Test
	public void testSuccessValidValue() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessInvalidSingleRuleWithoutCritical() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessInvalidMultipleRuleWithoutCritical() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testFailInvalidSingleRuleWithCritical() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("test")
				.critical();
		
		assertFalse(builder.apply("o"));
	}

	@Test
	public void testFailInvalidMultipleRuleWithCritical() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);

		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("test")
				.critical()
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test");

		assertFalse(builder.apply("o"));
	}


	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidSingleRuleWithCriticalException() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("test")
				.critical(ValidationSampleException.class);
		
		assertFalse(builder.apply("o"));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidMultipleRuleWithCriticalException() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test")
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("test")
				.critical(ValidationSampleException.class)
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertFalse(builder.apply("o"));
	}

	@Test
	public void testSuccessValidAndInvalidMultipleRule() {
		
		final RuleBuilderProperty<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
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
		
		assertTrue(builder.apply("o"));
	}

}
