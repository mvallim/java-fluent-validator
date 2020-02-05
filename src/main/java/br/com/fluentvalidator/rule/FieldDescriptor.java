package br.com.fluentvalidator.rule;

interface FieldDescriptor<T, P> {

  String getMessage(final T instance);

  String getCode(final T instance);

  String getFieldName(final T instance);

  Object getAttemptedValue(final T instance, final P defaultValue);

}
