/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.com.fluentvalidator;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.context.ProcessorContext;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.rule.Rule;
import br.com.fluentvalidator.rule.RuleBuilderCollectionImpl;
import br.com.fluentvalidator.rule.RuleBuilderPropertyImpl;
import br.com.fluentvalidator.rule.RuleProcessorStrategy;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public abstract class AbstractValidator<T> implements Validator<T> {

  private final List<Rule<T>> rules = new LinkedList<>();

  private final Initializer<T> initialize;

  private String property;

  private RuleProcessorStrategy ruleProcessor = RuleProcessorStrategy.getDefault();

  private static class Initializer<T> {

    private final AtomicReference<Boolean> atomicReference = new AtomicReference<>(Boolean.FALSE);

    private final Validator<T> validator;

    Initializer(final Validator<T> validator) {
      this.validator = validator;
    }

    /**
     * This method cause Race Condition. We are using Compare And Swap (CAS)
     * <p>
     * {@link https://en.wikipedia.org/wiki/Race_condition}
     * {@link https://en.wikipedia.org/wiki/Compare-and-swap}
     */
    public void init() {
      if (isNotInitialized()) {
        synchronized (atomicReference) {
          if (isNotInitialized()) { // double check if was initialized
            validator.rules();
            final Boolean oldValue = atomicReference.get();
            final Boolean newValue = Boolean.TRUE;
            atomicReference.compareAndSet(oldValue, newValue);
          }
        }
      }
    }

    private boolean isNotInitialized() {
      return Boolean.FALSE.equals(atomicReference.get());
    }

  }

  protected AbstractValidator() {
    this.initialize = new Initializer<>(this);
  }

  /**
   * {@link #failFastRule() AbstractValidator}
   */
  @Override
  public void failFastRule() {
    this.ruleProcessor = RuleProcessorStrategy.getFailFast();
  }

  /**
   * {@link #getCounter() AbstractValidator}
   */
  @Override
  public Integer getCounter() {
    return ProcessorContext.get().get();
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
    ruleProcessor.process(instance, this);
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
    return Collections.unmodifiableList(instances.stream().map(this::validate).collect(Collectors.toList()));
  }

  /**
   * {@link #validate(Collection, ValidationResultTransform) AbstractValidator}
   */
  @Override
  public <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> resultTransform) {
    return Collections.unmodifiableList(instances.stream().map(instance -> this.validate(instance, resultTransform)).collect(Collectors.toList()));
  }

  /**
   * {@link #apply(Object) AbstractValidator}
   */
  @Override
  public boolean apply(final T instance) {
    this.initialize.init();
    ValidationContext.get().setProperty(this.property, instance);
    return ruleProcessor.process(instance, instance, rules);
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
   * {@link #ruleFor(String, Function) AbstractValidator}
   */
  @Override
  public <P> RuleBuilderProperty<T, P> ruleFor(final String fieldName, final Function<T, P> function) {
    final RuleBuilderPropertyImpl<T, P> rule = new RuleBuilderPropertyImpl<>(fieldName, function);
    this.rules.add(rule);
    return rule;
  }

  /**
   * {@link #ruleForEach(String, Function) AbstractValidator}
   */
  @Override
  public <P> RuleBuilderCollection<T, P> ruleForEach(final String fieldName, final Function<T, Collection<P>> function) {
    final RuleBuilderCollectionImpl<T, P> rule = new RuleBuilderCollectionImpl<>(fieldName, function);
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
