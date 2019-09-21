package br.com.fluentvalidator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.rule.Rule;
import br.com.fluentvalidator.rule.RuleBuilderCollectionImpl;
import br.com.fluentvalidator.rule.RuleBuilderPropertyImpl;
import br.com.fluentvalidator.rule.RuleProcessor;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public abstract class AbstractValidator<T> implements Validator<T> {

  private final List<Rule<T>> rules = new LinkedList<>();

  private final Runnable initialize;

  private String property;

  protected AbstractValidator() {
    this.initialize = new Runnable() {

      private boolean initialized;

      @Override
      public synchronized void run() {
        if (!initialized) {
          rules();
          this.initialized = true;
        }
      }

    };
  }

  /**
   * {@link #setPropertyOnContext(String) AbstractValidator }
   */
  @Override
  public void setPropertyOnContext(final String property) {
    this.property = property;
  }

  /**
   * {@link #getPropertyOnContext(String, Class) AbstractValidator }
   */
  @Override
  public <P> P getPropertyOnContext(final String property, final Class<P> clazz) {
    return ValidationContext.get().getProperty(property, clazz);
  }

  /**
   * {@link #validate(Object) AbstractValidator }
   */
  @Override
  public ValidationResult validate(final T instance) {
    RuleProcessor.process(instance, this);
    return ValidationContext.get().getValidationResult();
  }

  /**
   * {@link #validate(Object, ValidationResultTransform) AbstractValidator}
   */
  @Override
  public <E> E validate(final T instance, final ValidationResultTransform<E> resultTransform) {
    return resultTransform.transform(validate(instance));
  }

  /**
   * {@link #validate(Collection) AbstractValidator}
   */
  @Override
  public List<ValidationResult> validate(final Collection<T> instances) {
    final List<ValidationResult> results = new ArrayList<>();
    for (final T instance : instances) {
      results.add(this.validate(instance));
    }
    return Collections.unmodifiableList(results);
  }

  /**
   * {@link #validate(Collection, ValidationResultTransform) AbstractValidator}
   */
  @Override
  public <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> resultTransform) {
    final List<E> results = new ArrayList<>();
    for (final ValidationResult validationResult : validate(instances)) {
      results.add(resultTransform.transform(validationResult));
    }
    return results;
  }

  /**
   * {@link #apply(Object) AbstractValidator}
   */
  @Override
  public boolean apply(final T instance) {
    this.initialize.run();
    ValidationContext.get().setProperty(this.property, instance);
    return RuleProcessor.process(instance, rules);
  }

  /**
   * {@link #support(Object) AbstractValidator}
   */
  @Override
  public boolean support(final T instance) {
    return true;
  }

  /**
   * {@link #ruleFor(Function) AbstractValidator}
   */
  @Override
  public <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function) {
    final RuleBuilderPropertyImpl<T, P> rule = new RuleBuilderPropertyImpl<>(function);
    this.rules.add(rule);
    return rule;
  }

  /**
   * {@link #ruleForEach(Function) AbstractValidator}
   */
  @Override
  public <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function) {
    final RuleBuilderCollectionImpl<T, P> rule = new RuleBuilderCollectionImpl<>(function);
    this.rules.add(rule);
    return rule;
  }

}