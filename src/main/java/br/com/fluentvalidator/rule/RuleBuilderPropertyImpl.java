package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.builder.Code;
import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.builder.WhenProperty;
import br.com.fluentvalidator.builder.WithValidator;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.exception.ValidationException;

public class RuleBuilderPropertyImpl<T, P> extends AbstractRuleBuilder<T, P, WhenProperty<T, P>> implements RuleBuilderProperty<T, P>, WhenProperty<T, P>, Rule<T> {

  private final Collection<Rule<P>> rules = new LinkedList<>();

  private final RuleProcessorStrategy ruleProcessor = RuleProcessorStrategy.getFailFast();

  private ValidationRule<P, P> currentValidation;

  public RuleBuilderPropertyImpl(final String fieldName, final Function<T, P> function) {
    super(fieldName, function);
  }

  public RuleBuilderPropertyImpl(final Function<T, P> function) {
    super(function);
  }

  @Override
  public boolean apply(final T instance) {
    final P value = Objects.nonNull(instance) ? function.apply(instance) : null;
    return ruleProcessor.process(value, rules);
  }

  @Override
  public WhenProperty<T, P> whenever(final Predicate<P> whenever) {
    this.currentValidation = new ValidatorRuleInternal(fieldName, whenever);
    this.rules.add(this.currentValidation);
    return this;
  }

  @Override
  public Must<T, P, WhenProperty<T, P>> must(final Predicate<P> must) {
    this.currentValidation = new ValidationRuleInternal(fieldName, must);
    this.rules.add(this.currentValidation);
    return this;
  }

  @Override
  public Message<T, P, WhenProperty<T, P>> withMessage(final String message) {
    this.currentValidation.withMessage(message);
    return this;
  }

  @Override
  public Code<T, P, WhenProperty<T, P>> withCode(final String code) {
    this.currentValidation.withCode(code);
    return this;
  }

  @Override
  public FieldName<T, P, WhenProperty<T, P>> withFieldName(final String fieldName) {
    this.currentValidation.withFieldName(fieldName);
    return this;
  }

  @Override
  public Critical<T, P, WhenProperty<T, P>> critical() {
    this.currentValidation.critical();
    return this;
  }

  @Override
  public Critical<T, P, WhenProperty<T, P>> critical(final Class<? extends ValidationException> clazz) {
    this.currentValidation.critical(clazz);
    return this;
  }

  @Override
  public WithValidator<T, P, WhenProperty<T, P>> withValidator(final Validator<P> validator) {
    this.currentValidation.withValidator(validator);
    return this;
  }

  @Override
  public WhenProperty<T, P> when(final Predicate<P> predicate) {
    this.currentValidation.when(predicate);
    return this;
  }

  @Override
  public boolean support(final T instance) {
    return true;
  }

  class ValidationRuleInternal extends AbstractValidationRule<P, P> {

    ValidationRuleInternal(final String fieldName, final Predicate<P> must) {
      super.must(must);
      super.withFieldName(fieldName);
    }

    @Override
    public boolean support(final P instance) {
      return Boolean.TRUE.equals(getWhen().test(instance));
    }

    @Override
    public boolean apply(final P instance) {

      final boolean apply = getMust().test(instance);

      if (Boolean.FALSE.equals(apply)) {
        ValidationContext.get().addError(getFieldName(), getMessage(), getCode(), instance);
      }

      if (Objects.nonNull(getCriticalException()) && Boolean.FALSE.equals(apply)) {
        throw ValidationException.create(getCriticalException());
      }

      return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
    }

  }

  class ValidatorRuleInternal extends AbstractValidationRule<P, P> {

    ValidatorRuleInternal(final String fieldName, final Predicate<P> whenever) {
      super.whenever(whenever);
      super.withFieldName(fieldName);
    }

    @Override
    public boolean support(final P instance) {
      return Boolean.TRUE.equals(getWhenever().test(instance));
    }

    @Override
    public boolean apply(final P instance) {

      final boolean apply = ruleProcessor.process(instance, getValidator());

      if (Objects.nonNull(getCriticalException()) && Boolean.FALSE.equals(apply)) {
        throw ValidationException.create(getCriticalException());
      }

      return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
    }

  }

}
