package br.com.fluentvalidator.predicate;

class ObjectFrom<T> {

  private final T source;

  private final T target;

  public ObjectFrom(final T source, final T target) {
    this.source = source;
    this.target = target;
  }

  public T getSource() {
    return source;
  }

  public T getTarget() {
    return target;
  }

}
