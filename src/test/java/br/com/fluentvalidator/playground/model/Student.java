package br.com.fluentvalidator.playground.model;

public class Student {
  private final String enrolmentId;
  private final String id;

  public Student(final Builder builder) {
    enrolmentId = builder.enrolmentId;
    id = builder.id;
  }

  public String getId() {
    return id;
  }

  public String getEnrolmentId() {
    return enrolmentId;
  }

  public static class Builder {

    private String enrolmentId;
    private String id;

    public static Builder newInstance() {
      return new Student.Builder();
    }

    public Builder setEnrolmentId(final String enrolmentId) {
      this.enrolmentId = enrolmentId;
      return this;
    }

    public Builder setId(final String id) {
      this.id = id;
      return this;
    }

    public Student build() {
      return new Student(this);
    }

  }

}
