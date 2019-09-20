package br.com.fluentvalidator.model;

public class Boy extends Child {

  private final Gender gender = Gender.MALE;

  public Boy(final String name, final int age) {
    super(name, age);
  }

  public Gender getGender() {
    return gender;
  }

}
