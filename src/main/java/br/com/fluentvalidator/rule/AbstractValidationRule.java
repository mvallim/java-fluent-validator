package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.context.Error;
import br.com.fluentvalidator.exception.ValidationException;
import br.com.fluentvalidator.handler.HandlerInvalidField;

@SuppressWarnings("unchecked")
abstract class AbstractValidationRule<T, P> implements ValidationRule<T, P>, FieldDescriptor<Object, P> {

  private Predicate<P> whenever = w -> true;

  private Predicate<P> when = w -> true;

  private Predicate<P> must = m -> true;

  private Function<Object, String> message = obj -> null;

  private Function<Object, String> code = obj -> null;

  private Function<Object, String> fieldName = obj -> null;

  private Function<Object, Object> attemptedValue;

  private boolean critical;

  private Class<? extends ValidationException> criticalException;

  private Validator<T> validator;

  private HandlerInvalidField<P> handlerInvalidField = new InternalHandlerInvalidField(this);

  public Predicate<P> getWhenever() {
    return this.whenever;
  }

  public Predicate<P> getWhen() {
    return this.when;
  }

  public Predicate<P> getMust() {
    return this.must;
  }

  public Class<? extends ValidationException> getCriticalException() {
    return this.criticalException;
  }

  public Validator<T> getValidator() {
    return this.validator;
  }

  @Override
  public String getMessage(final Object instance) {
    return this.message.apply(instance);
  }

  @Override
  public String getCode(final Object instance) {
    return this.code.apply(instance);
  }

  @Override
  public String getFieldName(final Object instance) {
    return this.fieldName.apply(instance);
  }

  @Override
  public Object getAttemptedValue(final Object instance, final P defaultValue) {
    return Objects.isNull(this.attemptedValue) ? defaultValue : this.attemptedValue.apply(instance);
  }

  public HandlerInvalidField<P> getHandlerInvalid() {
    return handlerInvalidField;
  }

  public boolean isCritical() {
    return this.critical;
  }

  @Override
  public void when(final Predicate<P> when) {
    this.when = when;
  }

  @Override
  public void must(final Predicate<P> must) {
    this.must = must;
  }

  @Override
  public void withFieldName(final Function<?, String> fieldName) {
    this.fieldName = (Function<Object, String>) fieldName;
  }

  @Override
  public void withMessage(final Function<?, String> message) {
    this.message = (Function<Object, String>) message;
  }

  @Override
  public void withCode(final Function<?, String> code) {
    this.code = (Function<Object, String>) code;
  }

  @Override
  public void withAttemptedValue(final Function<?, Object> attemptedValue) {
    this.attemptedValue = (Function<Object, Object>) attemptedValue;
  }

  @Override
  public void withHandlerInvalidField(final HandlerInvalidField<P> handlerInvalidField) {
    this.handlerInvalidField = handlerInvalidField;
  }

  @Override
  public void critical() {
    this.critical = true;
  }

  @Override
  public void critical(final Class<? extends ValidationException> clazz) {
    this.critical = true;
    this.criticalException = clazz;
  }

  @Override
  public void whenever(final Predicate<P> whenever) {
    this.whenever = whenever;
  }

  @Override
  public void withValidator(final Validator<T> validator) {
    this.validator = validator;
  }

  private class InternalHandlerInvalidField implements HandlerInvalidField<P> {

    private final FieldDescriptor<Object, P> fieldDescriptor;

    public InternalHandlerInvalidField(final FieldDescriptor<Object, P> fieldDescriptor) {
      this.fieldDescriptor = fieldDescriptor;
    }

    @Override
    public Collection<Error> handle(final Object instance, final P attemptedValue) {
      return Collections.singletonList(Error.create(fieldDescriptor.getFieldName(instance),
          fieldDescriptor.getMessage(instance), fieldDescriptor.getCode(instance),
          fieldDescriptor.getAttemptedValue(instance, attemptedValue)));
    }

  }

}
