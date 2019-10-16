package br.com.fluentvalidator.rule;

interface FieldDescriptor<T, P> {

  String getMessage(final T instance);

  String getCode(final T instance);

  String getFieldName(final T instance);

  P getAttemptedValue(final T instance, final P defaultValue);

}
