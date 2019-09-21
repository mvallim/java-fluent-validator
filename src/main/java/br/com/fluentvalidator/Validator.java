package br.com.fluentvalidator;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.RuleBuilderProperty;
import br.com.fluentvalidator.context.ValidationResult;
import br.com.fluentvalidator.rule.Rule;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public interface Validator<T> extends Rule<T> {

  /**
   *
   * @param instance
   * @return
   */
  ValidationResult validate(final T instance);

  /**
   *
   * @param instance
   * @param transform
   * @return
   */
  <E> E validate(final T instance, final ValidationResultTransform<E> transform);

  /**
   *
   * @param instances
   * @return
   */
  List<ValidationResult> validate(final Collection<T> instances);

  /**
   *
   * @param instances
   * @param transform
   * @return
   */
  <E> List<E> validate(final Collection<T> instances, final ValidationResultTransform<E> transform);

  /**
   *
   * @param function
   * @return
   */
  <P> RuleBuilderProperty<T, P> ruleFor(final Function<T, P> function);

  /**
   *
   * @param function
   * @return
   */
  <P> RuleBuilderCollection<T, P> ruleForEach(final Function<T, Collection<P>> function);

}
