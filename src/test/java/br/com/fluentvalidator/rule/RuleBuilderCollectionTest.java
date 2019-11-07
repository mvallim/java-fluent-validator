package br.com.fluentvalidator.rule;

import static br.com.fluentvalidator.predicate.CollectionPredicate.hasSize;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isFalse;
import static br.com.fluentvalidator.predicate.LogicalPredicate.isTrue;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSizeLessThan;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.junit.After;
import org.junit.Test;
import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.exception.ValidationSampleException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

public class RuleBuilderCollectionTest {

  @After
  public void tearDown() {
    ValidationContext.remove();
  }

  @Test
  public void testFailWhenApplyNullValue() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).withMessage("test").critical();

    assertFalse(builder.apply(null));
  }

  @Test
  public void testSuccessValidValue() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).when(not(nullValue())).withMessage("test");

    assertTrue(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testSuccessInvalidSingleRuleWithoutCritical() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(1)).when(not(nullValue())).withMessage("test");

    assertTrue(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testSuccessInvalidMultipleRuleWithoutCritical() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).when(not(nullValue())).withMessage("test").must(hasSize(2))
        .when(not(nullValue())).withMessage("test").must(hasSize(1)).when(not(nullValue()))
        .withMessage("test").must(hasSize(2)).when(not(nullValue())).withMessage("test");

    assertTrue(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testSuccessDynamicProperties() {

    final RuleBuilderCollectionImpl<List<String>, String> builder = new RuleBuilderCollectionImpl<>("test", Collections::unmodifiableList);

    builder
      .must(hasSize(1))
        .withMessage(List::toString)
      .must(hasSize(1))
        .withCode(List::toString)
      .must(hasSize(1))
        .withFieldName(List::toString)
      .must(hasSize(1))
        .withAttempedValue(me -> me)
      .must(hasSize(1))
        .withAttempedValue(Collections.emptyList())
      .must(hasSize(1))
        .when(not(nullValue()))
        .handlerInvalidField(new HandlerInvalidField<Collection<String>>() {
          public Collection<Error> handle(final Collection<String> attemptedValue) {
            return Collections.emptyList();
          };
        });

    assertTrue(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testSuccessInvalidSingleRuleWithCritical() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).when(not(nullValue())).withMessage("test").critical();

    assertTrue(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testFailInvalidSingleRuleWithCritical() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(1)).when(not(nullValue())).withMessage("test").critical();

    assertFalse(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testSuccessInvalidSingleRuleWithCriticalException() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).when(not(nullValue())).withMessage("test")
        .critical(ValidationSampleException.class);

    assertTrue(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testFailInvalidSingleRuleWithCriticalException() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(1)).when(not(nullValue())).withMessage("test")
        .critical(ValidationSampleException.class);

    final Throwable throwable = catchThrowable(() -> builder.apply(Arrays.asList("o", "oo")));

    assertThat(throwable).isInstanceOf(ValidationSampleException.class);
  }

  @Test
  public void testFailRuleValidator() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.whenever(not(nullValue())).withValidator(new ValidatorIdTest());

    assertTrue(builder.apply(Arrays.asList("")));
  }

  @Test
  public void testFailRuleValidatorWithCritical() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.whenever(not(nullValue())).withValidator(new ValidatorIdTest()).critical();

    assertFalse(builder.apply(Arrays.asList("oo")));
  }

  @Test
  public void testFailRuleValidatorWithCriticalException() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.whenever(not(nullValue())).withValidator(new ValidatorIdTest())
        .critical(ValidationSampleException.class);

    final Throwable throwable = catchThrowable(() -> builder.apply(Arrays.asList("o")));

    assertThat(throwable).isInstanceOf(ValidationSampleException.class);
  }

  @Test
  public void testFailInvalidMultipleRuleWithCritical() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).when(not(nullValue())).withMessage("test").must(hasSize(2))
        .when(not(nullValue())).withMessage("test").must(hasSize(1)).when(not(nullValue()))
        .withMessage("test").critical().must(hasSize(2)).when(not(nullValue())).withMessage("test");

    assertFalse(builder.apply(Arrays.asList("o", "oo")));
  }

  @Test
  public void testFailInvalidSingleWithCriticalException() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(1)).when(not(nullValue())).withMessage("test")
        .critical(ValidationSampleException.class);

    final Throwable throwable = catchThrowable(() -> builder.apply(Arrays.asList("o", "oo")));

    assertThat(throwable).isInstanceOf(ValidationSampleException.class);
  }

  @Test
  public void testFailInvalidMultipleWithCriticalException() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(hasSize(2)).when(not(nullValue())).withMessage("test").must(hasSize(2))
        .when(not(nullValue())).withMessage("test").must(hasSize(1)).when(not(nullValue()))
        .withMessage("test").critical(ValidationSampleException.class).must(hasSize(2))
        .when(not(nullValue())).withMessage("test");

    final Throwable throwable = catchThrowable(() -> builder.apply(Arrays.asList("o", "oo")));

    assertThat(throwable).isInstanceOf(ValidationSampleException.class);
  }

  @Test
  public void testSuccessValidAndInvalidMultipleRule() {

    final RuleBuilderCollectionImpl<List<String>, String> builder =
        new RuleBuilderCollectionImpl<>(Collections::unmodifiableList);

    builder.must(isFalse()).when(isTrue()).withMessage("ever enter here").withCode("666")
        .withFieldName("size").must(isTrue()).when(isTrue()).withMessage("never enter here")
        .withCode("666").withFieldName("size").must(isTrue()).when(isFalse())
        .withMessage("never enter here").withCode("666").withFieldName("size").must(isFalse())
        .when(isFalse()).withMessage("never enter here").withCode("666").withFieldName("size");

    assertTrue(builder.apply(Arrays.asList("o")));
  }

  class ValidatorIdTest extends AbstractValidator<String> {

    @Override
    public void rules() {

      ruleFor(id -> id).must(stringSizeLessThan(2)).withMessage("rule 1").critical()
          .must(stringSizeLessThan(1)).withMessage("rule 2").critical();
    }

  }
}
