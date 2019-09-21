package br.com.fluentvalidator.context;

import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

public final class Context {

  private final Map<String, Object> properties = new ConcurrentHashMap<>();

  private final Queue<Error> errors = new ConcurrentLinkedQueue<>();

  /**
   *
   * @param field
   * @param message
   * @param code
   * @param attemptedValue
   */
  public void addError(final String field, final String message, final String code, final Object attemptedValue) {
    errors.add(Error.create(field, message, code, attemptedValue));
  }

  /**
   *
   * @param property
   * @param value
   */
  public void setProperty(final String property, final Object value) {
    if (Objects.nonNull(property)) {
      properties.put(property, value);
    }
  }

  /**
   *
   * @param property
   * @param clazz
   * @return
   */
  public <P> P getProperty(final String property, final Class<P> clazz) {
    return clazz.cast(properties.getOrDefault(property, null));
  }

  /**
   *
   * @return
   */
  public ValidationResult getValidationResult() {
    ValidationContext.remove();
    return errors.isEmpty() ? ValidationResult.ok() : ValidationResult.fail(errors);
  }

}