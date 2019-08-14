package br.com.fluentvalidator.test;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.junit.Test;

import br.com.fluentvalidator.ValidationResult;
import br.com.fluentvalidator.builder.Validator;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.model.Parent;
import br.com.fluentvalidator.validator.ValidatorParent;

public class ValidatorTest {

	@Test
	public void validationMustBeSuccess() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(6);
		parent.setName("John Gow");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent.setChildren(Arrays.asList(new Boy("John", 5), new Girl("Ana", 5)));

		final ValidationResult result = validatorParent.validate(parent);

		assertTrue(result.isValid());
		assertThat(result.getErrors(), empty());
	}

	@Test
	public void validationMustBeFailWhenFieldOfParentAreInvalid() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(10);
		parent.setName("Ana");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
		parent.setChildren(Arrays.asList(new Boy("John", 5)));

		final ValidationResult result = validatorParent.validate(parent);

		assertFalse(result.isValid());
		assertThat(result.getErrors(), not(empty()));
		assertThat(result.getErrors(), hasSize(3));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(10))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
		
		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
	}
	
	@Test
	public void validationMustBeFailWhenChildAgeGreateThanParentAgeInvalid() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(6);
		parent.setName("John Gow");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent.setChildren(Arrays.asList(new Boy("John", 6)));

		final ValidationResult result = validatorParent.validate(parent);

		assertFalse(result.isValid());
		assertThat(result.getErrors(), not(empty()));
		assertThat(result.getErrors(), hasSize(1));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(6))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be less than age parent"))));
	}

	@Test
	public void validationTwiceDiferentParentMustBeSuccess() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent1 = new Parent();

		parent1.setAge(6);
		parent1.setName("John Gow");
		parent1.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent1.setChildren(Arrays.asList(new Boy("John", 5)));

		final Parent parent2 = new Parent();

		parent2.setAge(10);
		parent2.setName("Ana");
		parent2.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
		parent2.setChildren(Arrays.asList(new Boy("John", 5)));

		final ValidationResult result1 = validatorParent.validate(parent1);
		final ValidationResult result2 = validatorParent.validate(parent2);

		assertTrue(result1.isValid());
		assertThat(result1.getErrors(), empty());

		assertFalse(result2.isValid());
		assertThat(result2.getErrors(), not(empty()));
		assertThat(result2.getErrors(), hasSize(3));

		assertThat(result2.getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(10))));
		assertThat(result2.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));

		assertThat(result2.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent2.getCities()))));
		assertThat(result2.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
		
		assertThat(result2.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
	}

	@Test
	public void validationCollectionParentMustBeSuccess() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent1 = new Parent();

		parent1.setAge(6);
		parent1.setName("John Gow");
		parent1.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent1.setChildren(Arrays.asList(new Boy("John", 5)));

		final Parent parent2 = new Parent();

		parent2.setAge(10);
		parent2.setName("Ana");
		parent2.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
		parent2.setChildren(Arrays.asList(new Boy("John", 5)));

		final List<ValidationResult> result = validatorParent.validate(Arrays.asList(parent1, parent2));

		assertTrue(result.get(0).isValid());
		assertThat(result.get(0).getErrors(), empty());

		assertFalse(result.get(1).isValid());
		assertThat(result.get(1).getErrors(), not(empty()));
		assertThat(result.get(1).getErrors(), hasSize(3));

		assertThat(result.get(1).getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("attemptedValue", equalTo(10))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));

		assertThat(result.get(1).getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent2.getCities()))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
		
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
	}

	@Test
	public void validationMustBeFalseWhenChildrenIsNull() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(6);
		parent.setName("John Gow");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));

		final ValidationResult result = validatorParent.validate(parent);

		assertFalse(result.isValid());
		assertThat(result.getErrors(), not(empty()));
		assertThat(result.getErrors(), hasSize(1));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("children"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", nullValue())));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("parent's children cannot be null"))));
	}

	@Test
	public void validationMustBeFalseWhenChildrenIsEmpty() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(6);
		parent.setName("John Gow");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent.setChildren(new ArrayList<>());

		final ValidationResult result = validatorParent.validate(parent);
		
		assertFalse(result.isValid());
		assertThat(result.getErrors(), not(empty()));
		assertThat(result.getErrors(), hasSize(1));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("children"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", empty())));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("parent must have at least one child"))));
	}
	
	@Test
	public void validationMustBeFalseWhenChildrenIsInvalid() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(6);
		parent.setName("John Gow");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent.setChildren(Arrays.asList(new Girl("Barbara", 4)));

		final ValidationResult result = validatorParent.validate(parent);
		
		assertFalse(result.isValid());
		assertThat(result.getErrors(), not(empty()));
		assertThat(result.getErrors(), hasSize(2));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Barbara"))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key Ana"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(4))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be greater than or equal to 5"))));
	}
	
	@Test
	public void validationMustBeFalseWhenParentAndChildrenIsInvalid() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(10);
		parent.setName("Ana");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
		parent.setChildren(Arrays.asList(new Girl("Barbara", 4)));

		final ValidationResult result = validatorParent.validate(parent);
		
		assertFalse(result.isValid());
		assertThat(result.getErrors(), not(empty()));
		assertThat(result.getErrors(), hasSize(5));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(10))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(4))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be greater than or equal to 5"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Barbara"))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child name must contains key Ana"))));	
	}	

	@Test
	public void validationMultiThreadMustBeTrue() throws InterruptedException {
		for (int i = 0; i < 10; i++) {
			final Parent parentOne = new Parent();

			parentOne.setAge(6);
			parentOne.setName("John Gow");
			parentOne.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
			parentOne.setChildren(Arrays.asList(new Boy("John", 5)));

			final ThreadLocalTest runnableOne = new ThreadLocalTest(parentOne);
			final Thread threadOne = new Thread(runnableOne);

			final Parent parentTwo = new Parent();

			parentTwo.setAge(10);
			parentTwo.setName("Ana");
			parentTwo.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
			parentTwo.setChildren(Arrays.asList(new Boy("John", 5)));

			final ThreadLocalTest runnableTwo = new ThreadLocalTest(parentTwo);
			final Thread threadTwo = new Thread(runnableTwo);

			threadOne.start();
			threadTwo.start();

			threadOne.join();
			threadTwo.join();

			final ValidationResult resultOne = runnableOne.results();
			final ValidationResult resultTwo = runnableTwo.results();

			assertTrue(resultOne.isValid());
			assertThat(resultOne.getErrors(), empty());

			assertFalse(resultTwo.isValid());
			assertThat(resultTwo.getErrors(), not(empty()));
			assertThat(resultTwo.getErrors(), hasSize(3));

			assertThat(resultTwo.getErrors(), hasItem(hasProperty("field", containsString("age"))));
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(10))));
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));

			assertThat(resultTwo.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parentTwo.getCities()))));
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
			
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("field", containsString("name"))));
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
			assertThat(resultTwo.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));	
		}
	}

	class ThreadLocalTest implements Runnable {

		private final Validator<Parent> validatorParent = new ValidatorParent();

		private final Parent parent;

		private ValidationResult results;

		public ThreadLocalTest(Parent parent) {
			this.parent = parent;
		}

		@Override
		public void run() {
			try {
				final Random rand = new Random();
				Thread.sleep(rand.nextInt(100));
				this.results = this.validatorParent.validate(this.parent);
			} catch (final InterruptedException e) {
				e.printStackTrace();
			}
		}

		public ValidationResult results() {
			return this.results;
		}

	}
}
