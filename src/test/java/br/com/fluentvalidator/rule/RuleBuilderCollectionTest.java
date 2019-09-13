package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static br.com.fluentvalidator.predicate.ComparablePredicate.lessThan;
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

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.exception.ValidationSampleException;

public class RuleBuilderCollectionTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}
	
	@Test
	public void testFailWhenApplyNullValue() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertFalse(builder.apply(null));
	}
	
	@Test
	public void testSuccessValidValue() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testSuccessInvalidSingleRuleWithoutCritical() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(1))
				.when(not(nullValue()))
				.withMessage("test");
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}
	
	@Test
	public void testSuccessInvalidMultipleRuleWithoutCritical() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
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
	public void testSuccessInvalidSingleRuleWithCritical() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))		
				.withMessage("test")
				.critical();
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testFailInvalidSingleRuleWithCritical() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(1))
				.when(not(nullValue()))		
				.withMessage("test")
				.critical();
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}
	
	@Test
	public void testSuccessInvalidSingleRuleWithCriticalException() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(2))
				.when(not(nullValue()))		
				.withMessage("test")
				.critical(ValidationSampleException.class);
		
		assertTrue(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidSingleRuleWithCriticalException() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(1))
				.when(not(nullValue()))		
				.withMessage("test")
				.critical(ValidationSampleException.class);
		
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test
	public void testFailRuleValidator() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.whenever(not(nullValue()))
				.withValidator(new ValidatorIdTest());
		
		assertTrue(builder.apply(""));
	}
	
	@Test
	public void testFailRuleValidatorWithCritical() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.whenever(not(nullValue()))
				.withValidator(new ValidatorIdTest())
				.critical();
		
		assertFalse(builder.apply("oo"));
	}
	
	@Test(expected = ValidationSampleException.class)
	public void testFailRuleValidatorWithCriticalException() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.whenever(not(nullValue()))
				.withValidator(new ValidatorIdTest())
				.critical(ValidationSampleException.class);
		
		assertFalse(builder.apply("o"));
	}	

	@Test
	public void testFailInvalidMultipleRuleWithCritical() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
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
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
		builder
			.must(hasSize(1))
				.when(not(nullValue()))		
				.withMessage("test")
				.critical(ValidationSampleException.class);
						
		assertFalse(builder.apply(Arrays.asList("o", "oo")));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidMultipleWithCriticalException() {
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
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
		
		final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>(listStr -> listStr);
		
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
	
	class ValidatorIdTest extends AbstractValidator<Integer> {

		@Override
		protected void rules() {
			
			ruleFor(id -> id)
				.must(lessThan(2))
					.withMessage("rule 1")
					.critical()
				.must(lessThan(1))
					.withMessage("rule 2")
					.critical();
		}
		
	}
}
