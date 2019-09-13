package br.com.fluentvalidator.transform;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import br.com.fluentvalidator.ValidationResult;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.exception.Error;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.model.Parent;
import br.com.fluentvalidator.validator.ValidatorParent;

public class ValidationResultTransformTest {

	@Test
	public void validationTransformMustBeSuccess() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(6);
		parent.setName("John Gow");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
		parent.setChildren(Arrays.asList(new Boy("John", 5), new Girl("Ana", 5)));

		final String result = validatorParent.validate(parent, new ValidationResultTestTransform());

		assertThat(result, isEmptyString());
	}

	@Test
	public void validationTransformMustBeFailWhenFieldOfParentAreInvalid() {
		final Validator<Parent> validatorParent = new ValidatorParent();

		final Parent parent = new Parent();

		parent.setAge(10);
		parent.setName("Ana");
		parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
		parent.setChildren(Arrays.asList(new Boy("John", 5)));

		final String result = validatorParent.validate(parent, new ValidationResultTestTransform());

		assertThat(result, not(isEmptyString()));
		assertThat(result, containsString("age must be less than or equal to 7"));
		assertThat(result, containsString("cities size must be 10"));
		assertThat(result, containsString("name must contains key John"));
	}
	
	@Test
	public void validationTransformCollectionParentMustBeSuccess() {
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

		final List<String> result = validatorParent.validate(Arrays.asList(parent1, parent2), new ValidationResultTestTransform());

		assertThat(result.get(0), isEmptyString());

		assertThat(result.get(1), not(isEmptyString()));
		assertThat(result.get(1), containsString("age must be less than or equal to 7"));
		assertThat(result.get(1), containsString("cities size must be 10"));
		assertThat(result.get(1), containsString("name must contains key John"));
	}
	

	@Test
	public void validationTransformMultiThreadMustBeTrue() throws ExecutionException, InterruptedException {

		final int CONCURRENT_RUNNABLE = 100000;
		
		final ExecutorService executor = Executors.newFixedThreadPool(100);
		
		final List<String> cities = Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8");
		
		final Collection<String> resultsOne = new ConcurrentLinkedQueue<>();
		
		final Collection<String> resultsTwo = new ConcurrentLinkedQueue<>();

		for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {

			executor.submit(new Runnable() {
				@Override
				public void run() {
					final Validator<Parent> validatorParent = new ValidatorParent();

					final Parent parent = new Parent();

					parent.setAge(6);
					parent.setName("John Gow");
					parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"));
					parent.setChildren(Arrays.asList(new Boy("John", 5)));
					
					resultsOne.add(validatorParent.validate(parent, new ValidationResultTestTransform()));
				}
			});

			executor.submit(new Runnable() {
				@Override
				public void run() {

					final Validator<Parent> validatorParent = new ValidatorParent();

					final Parent parent = new Parent();

					parent.setAge(10);
					parent.setName("Ana");
					parent.setCities(cities);
					parent.setChildren(Arrays.asList(new Boy("John", 5)));

					resultsTwo.add(validatorParent.validate(parent,new ValidationResultTestTransform()));
				}
			});
		}

		executor.shutdown();

		executor.awaitTermination(10, TimeUnit.MINUTES);		
		
		assertThat(resultsOne, hasSize(CONCURRENT_RUNNABLE));
		assertThat(resultsTwo, hasSize(CONCURRENT_RUNNABLE));
		
		for (final String result : resultsOne) {
			assertThat(result, isEmptyString());			
		}

		for (final String result : resultsTwo) {
			assertThat(result, not(isEmptyString()));
			assertThat(result, containsString("age must be less than or equal to 7"));
			assertThat(result, containsString("cities size must be 10"));
			assertThat(result, containsString("name must contains key John"));			
		}
		
	}
	
	class ValidationResultTestTransform implements ValidationResultTransform<String> {

		@Override
		public String transform(final ValidationResult validationResult) {
			final StringBuilder sb = new StringBuilder();
			for (final Error error : validationResult.getErrors()) {
				sb.append(String.format("%s\n", error.getMessage()));
			}
			return sb.toString();
		}

	}

}
