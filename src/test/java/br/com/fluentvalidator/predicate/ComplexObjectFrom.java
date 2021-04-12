package br.com.fluentvalidator.predicate;

class ComplexObjectFrom<T, U> {

  private final T source;
  private final U target;

  public ComplexObjectFrom(T source, U target) {
    this.source = source;
    this.target = target;
  }

  public T getSource() {
    return source;
  }

  public U getTarget() {
    return target;
  }

}
