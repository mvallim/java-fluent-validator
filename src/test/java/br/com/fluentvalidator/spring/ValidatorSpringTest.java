package br.com.fluentvalidator.spring;

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
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import br.com.fluentvalidator.ValidationResult;
import br.com.fluentvalidator.exception.Error;
import br.com.fluentvalidator.exception.ValidationSampleException;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.model.Parent;
import br.com.fluentvalidator.spring.validator.ValidatorSpringParent;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = ValidatorSpringConfig.class)
public class ValidatorSpringTest {

	@Autowired
	ValidatorSpringParent validatorParent;
	
	@Test
	public void validationMustBeSuccess() {
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
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getAge()))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
		assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("666"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

		assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString(parent.getName()))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
	}
	
	@Test
	public void validationMustBeFailWhenFieldOfParentAreInvalidCriticalValidation() {
		try {
			final Parent parent = new Parent();
	
			parent.setId("invalid");
			parent.setAge(10);
			parent.setName("Ana");
			parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
			parent.setChildren(Arrays.asList(new Boy("John", 5)));
	
			validatorParent.validate(parent);
		} catch(Exception e) {
			assertThat(e.getMessage(), equalTo("Constructor in class not found (Collection<Error> errors)"));			
		}
	}

	@Test
	public void validationMustBeFailWhenChildAgeGreateThanParentAgeInvalid() {
		try {
			final Parent parent = new Parent();
	
			parent.setAge(6);
			parent.setName("John Gow");
			parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
			parent.setChildren(Arrays.asList(new Boy("John", 6)));
	
			validatorParent.validate(parent);
		} catch(ValidationSampleException e) {
			final List<Error> erros = new ArrayList<>(e.getErrors());
			assertThat(erros, hasSize(1));
			assertThat(erros.get(0).getCode(), equalTo(null));
			assertThat(erros.get(0).getMessage(), equalTo("child age must be less than age parent"));
			assertThat(erros.get(0).getAttemptedValue(), equalTo(6));
			assertThat(erros.get(0).getField(), equalTo("age"));
			assertThat(e.getMessage(), equalTo("[Error [message=child age must be less than age parent, field=age, attemptedValue=6, code=null]]"));			
		}
	}

	@Test
	public void validationTwiceDiferentParentMustBeSuccess() {
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
		assertThat(result2.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent2.getAge()))));
		assertThat(result2.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("code", containsString("666"))));

		assertThat(result2.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent2.getCities()))));
		assertThat(result2.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

		assertThat(result2.getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result2.getErrors(), hasItem(hasProperty("attemptedValue", containsString(parent2.getName()))));
		assertThat(result2.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
	}

	@Test
	public void validationCollectionParentMustBeSuccess() {
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
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent2.getAge()))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("code", containsString("666"))));

		assertThat(result.get(1).getErrors(), hasItem(hasProperty("field", containsString("cities"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent2.getCities()))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

		assertThat(result.get(1).getErrors(), hasItem(hasProperty("field", containsString("name"))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("attemptedValue", containsString(parent2.getName()))));
		assertThat(result.get(1).getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
	}

	@Test
	public void validationMustBeFalseWhenChildrenIsNull() {
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
		assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("555"))));
	}

	@Test
	public void validationMustBeFalseWhenChildrenIsEmpty() {
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
		assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getAge()))));
		assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
		assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("666"))));

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
	public void validationMustBeFalseWhenParentAndChildrenIsCriticalInvalid() {
		try {
			final Parent parent = new Parent();
	
			parent.setAge(6);
			parent.setName("John Gow");
			parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
			parent.setChildren(Arrays.asList(new Girl("Barbara", 6)));
	
			validatorParent.validate(parent);
		} catch(ValidationSampleException e) {
			final List<Error> erros = new ArrayList<>(e.getErrors());
			assertThat(erros, hasSize(1));
			assertThat(erros.get(0).getCode(), equalTo(null));
			assertThat(erros.get(0).getMessage(), equalTo("child age must be less than age parent"));
			assertThat(erros.get(0).getAttemptedValue(), equalTo(6));
			assertThat(erros.get(0).getField(), equalTo("age"));
			assertThat(e.getMessage(), equalTo("[Error [message=child age must be less than age parent, field=age, attemptedValue=6, code=null]]"));			
		}
	}	

	@Test
	public void validationMultiThreadMustBeTrue() throws ExecutionException, InterruptedException {

		final int CONCURRENT_RUNNABLE = 100000;
		
		final ExecutorService executor = Executors.newFixedThreadPool(100);
		
		final List<String> cities = Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8");
		
		final Collection<ValidationResult> resultsOne = new ConcurrentLinkedQueue<>();
		
		final Collection<ValidationResult> resultsTwo = new ConcurrentLinkedQueue<>();

		for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {

			executor.submit(new Runnable() {
				@Override
				public void run() {
					final Parent parent = new Parent();

					parent.setAge(6);
					parent.setName("John Gow");
					parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
					parent.setChildren(Arrays.asList(new Boy("John", 5)));
					
					resultsOne.add(validatorParent.validate(parent));
				}
			});

			executor.submit(new Runnable() {
				@Override
				public void run() {
					final Parent parent = new Parent();

					parent.setAge(10);
					parent.setName("Ana");
					parent.setCities(cities);
					parent.setChildren(Arrays.asList(new Boy("John", 5)));

					resultsTwo.add(validatorParent.validate(parent));
				}
			});
		}

		executor.shutdown();

		executor.awaitTermination(10, TimeUnit.MINUTES);		
		
		assertThat(resultsOne, hasSize(CONCURRENT_RUNNABLE));
		assertThat(resultsTwo, hasSize(CONCURRENT_RUNNABLE));
		
		for (final ValidationResult result : resultsOne) {
			assertTrue(result.isValid());
			assertThat(result.getErrors(), empty());				
		}

		for (final ValidationResult result : resultsTwo) {
			assertFalse(result.isValid());
			assertThat(result.getErrors(), not(empty()));
			assertThat(result.getErrors(), hasSize(3));

			assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
			assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(10))));
			assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
			assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("666"))));

			assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
			assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(cities))));
			assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

			assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
			assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
			assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));				
		}
		
	}

}
