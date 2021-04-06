package br.com.fluentvalidator;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
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

import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.model.Bill;
import br.com.fluentvalidator.model.Boy;
import br.com.fluentvalidator.model.Girl;
import br.com.fluentvalidator.model.Parent;
import br.com.fluentvalidator.validator.ValidatorBill;
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
    final Validator<Parent> validatorParent = new ValidatorParent();

    final Parent parent = new Parent();

    parent.setId("invalid");
    parent.setAge(10);
    parent.setName("Ana");
    parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
    parent.setChildren(Arrays.asList(new Boy("John", 5)));

    final ValidationResult result = validatorParent.validate(parent);

    assertFalse(result.isValid());
    assertThat(result.getErrors(), not(empty()));
    assertThat(result.getErrors(), hasSize(4));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("id"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getId()))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("id not matching the pattern of a UUID"))));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getName()))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getId()))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
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
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getAge()))));
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
    assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("555"))));
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
    final Validator<Parent> validatorParent = new ValidatorParent();

    final Parent parent = new Parent();

    parent.setAge(6);
    parent.setName("John Gow");
    parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
    parent.setChildren(Arrays.asList(new Girl("Barbara", 6)));

    final ValidationResult result = validatorParent.validate(parent);

    assertFalse(result.isValid());
    assertThat(result.getErrors(), not(empty()));
    assertThat(result.getErrors(), hasSize(3));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(6))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be less than age parent"))));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));

    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
    assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo("Barbara"))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child name must contains key Ana"))));
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
          final Validator<Parent> validatorParent = new ValidatorParent();

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

          final Validator<Parent> validatorParent = new ValidatorParent();

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

  @Test
  public void testSuccessWhenCriticalWasInDifferentRuleGroup() {

    final StringValidator validator = new StringValidator();

    final ValidationResult result = validator.validate("bla");

    assertFalse(result.isValid());
    assertThat(result.getErrors(), hasSize(3));

    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 1 rule 1"))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 2 rule 1"))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 3 rule 1"))));
  }

  @Test
  public void testSuccessWhenCriticalWasInDifferentRuleGroupFailFast() {

    final StringValidator validator = new StringValidator();

    validator.failFastRule();

    final ValidationResult result = validator.validate("bla");

    assertFalse(result.isValid());
    assertThat(result.getErrors(), hasSize(2));

    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 1 rule 1"))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 2 rule 1"))));
    assertThat(result.getErrors(), hasItem(hasProperty("message", not(containsString("group 3 rule 1")))));
  }

  @Test
  public void testFailWhenValidatePropertyNullValue() {

    final String2Validator validator = new String2Validator();

    final ValidationResult result = validator.validate((String) null);

    assertFalse(result.isValid());
    assertThat(result.getErrors(), hasSize(1));

    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 1 rule 1"))));
    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("string"))));
  }

  @Test
  public void testFailWhenValidateCollectionNullValue() {

    final String3Validator validator = new String3Validator();

    final ValidationResult result = validator.validate((Collection<String>) null);

    assertFalse(result.isValid());
    assertThat(result.getErrors(), hasSize(1));

    assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("group 1 rule 1"))));
    assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("collection"))));
  }

  @Test
  public void testSuccessWhenBillIsCorrect() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) 100.00, LocalDate.now().plusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertTrue(validate.isValid());
  }

  @Test
  public void testFailWhenBillDescriptionIsNull() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill(null, (float) 100.00, LocalDate.now().plusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), containsString("description is required"));
    assertThat(error.getField(), equalTo("description"));
  }

  @Test
  public void testFailWhenBillDescriptionIsEmpty() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("", (float) 100.00, LocalDate.now().plusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), containsString("description is required"));
    assertThat(error.getField(), equalTo("description"));
  }

  @Test
  public void testFailWhenBillValueIsNull() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", null, LocalDate.now().plusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), containsString("value must be provided"));
    assertThat(error.getField(), equalTo("value"));
  }

  @Test
  public void testFailWhenBillValueIsZero() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) 0, LocalDate.now().plusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), containsString("value must be greather than 0"));
    assertThat(error.getField(), equalTo("value"));
  }

  @Test
  public void testFailWhenBillValueIsNegative() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) -1, LocalDate.now().plusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), containsString("value must be greather than 0"));
    assertThat(error.getField(), equalTo("value"));
  }

  @Test
  public void testFailWhenBillDueDateIsToday() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) 100.00, LocalDate.now());

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), equalTo("Only future bills are allowed"));
    assertThat(error.getField(), equalTo("dueDate"));
  }

  @Test
  public void testFailWhenBillDueDateIsPast() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) 100.00, LocalDate.now().minusDays(1));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), equalTo("Only future bills are allowed"));
    assertThat(error.getField(), equalTo("dueDate"));
  }

  @Test
  public void testFailWhenBillDueDateIsFarTooAhead() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) 100.00, LocalDate.now().plusYears(4));

    final ValidationResult validate = validatorBill.validate(bill);

    assertFalse(validate.isValid());
    assertThat(validate.getErrors().size(), equalTo(1));

    final Error error = validate.getErrors().iterator().next();

    assertThat(error.getMessage(), equalTo("Max due date is 3 years ahead"));
    assertThat(error.getField(), equalTo("dueDate"));
  }

  @Test
  public void testSuccessWhenBillDueDateIsExactlyThreeYears() {
    final ValidatorBill validatorBill = new ValidatorBill();
    final Bill bill = new Bill("Energy bill", (float) 100.00, LocalDate.now().plusYears(3));

    final ValidationResult validate = validatorBill.validate(bill);

    assertTrue(validate.isValid());
  }

  class StringValidator extends AbstractValidator<String> {

    @Override
    public void rules() {

      ruleFor(str -> str).must(not(br.com.fluentvalidator.predicate.ComparablePredicate.equalTo("bla"))).withMessage("group 1 rule 1");

      ruleFor(str -> str).must(not(br.com.fluentvalidator.predicate.ComparablePredicate.equalTo("bla"))).withMessage("group 2 rule 1").critical().must(not(br.com.fluentvalidator.predicate.ComparablePredicate.equalTo("bla")))
          .withMessage("group 2 rule 2");

      ruleFor(str -> str).must(not(br.com.fluentvalidator.predicate.ComparablePredicate.equalTo("bla"))).withMessage("group 3 rule 1");

    }

  }

  class String2Validator extends AbstractValidator<String> {

    @Override
    public void rules() {

      ruleFor("string", str -> str).must(not(stringEmptyOrNull())).withMessage("group 1 rule 1");

    }

  }

  class String3Validator extends AbstractValidator<Collection<String>> {

    @Override
    public void rules() {

      ruleForEach("collection", str -> str).must(not(br.com.fluentvalidator.predicate.CollectionPredicate.empty())).withMessage("group 1 rule 1");

    }

  }

}
