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

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.ValidationContext;
import br.com.fluentvalidator.exception.ValidationSampleException;

public class RuleBuilderPropertyTest {
	
	@After
	public void tearDown() {
		ValidationContext.remove();
	}

	@Test
	public void testFailWhenApplyNullValue() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 1");
		
		assertFalse(builder.apply(null));
	}
	
	@Test
	public void testSuccessValidValue() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 1");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessInvalidSingleRuleWithoutCritical() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("rule 1");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessInvalidMultipleRuleWithoutCritical() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 1")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 2")
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("rule 3")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 4");
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testSuccessRuleWithCritical() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))		
				.withMessage("rule 1")
			.must(lessThan(2))
				.when(not(nullValue()))		
				.withMessage("rule 2")
				.critical();
		
		assertTrue(builder.apply("o"));
	}

	@Test
	public void testFailRuleWithCritical() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))		
				.withMessage("rule 1")
			.must(lessThan(1))
				.when(not(nullValue()))		
				.withMessage("rule 2")
				.critical();
		
		assertFalse(builder.apply("o"));
	}
	
	@Test
	public void testSuccessRuleWithCriticalException() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))		
				.withMessage("rule 1")
			.must(lessThan(2))
				.when(not(nullValue()))		
				.withMessage("rule 2")
				.critical(ValidationSampleException.class);
		
		assertTrue(builder.apply("o"));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailRuleWithCriticalException() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(1))
				.when(not(nullValue()))		
				.withMessage("rule 1")
			.must(lessThan(1))
				.when(not(nullValue()))		
				.withMessage("rule 2")
				.critical(ValidationSampleException.class);
		
		assertFalse(builder.apply("o"));
	}
	
	@Test
	public void testSuccessRuleValidator() {
		
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
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);

		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 1")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 2")
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("rule 3")
				.critical()
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 4");

		assertFalse(builder.apply("o"));
	}

	@Test(expected = ValidationSampleException.class)
	public void testFailInvalidMultipleRuleWithCriticalException() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
		builder
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 1")
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 2")
			.must(lessThan(1))
				.when(not(nullValue()))
				.withMessage("rule 3")
				.critical(ValidationSampleException.class)
			.must(lessThan(2))
				.when(not(nullValue()))
				.withMessage("rule 4");
		
		assertFalse(builder.apply("o"));
	}

	@Test
	public void testSuccessValidAndInvalidMultipleRule() {
		
		final RuleBuilderPropertyImpl<String, Integer> builder = new RuleBuilderPropertyImpl<>(String::length);
		
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
