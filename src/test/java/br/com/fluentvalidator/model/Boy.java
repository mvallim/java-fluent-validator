package br.com.fluentvalidator.model;

public class Boy extends Child {

	private final Gender gender =  Gender.MALE;

	public Boy(String name, int age) {
		super(name, age);
	}

	public Gender getGender() {
		return this.gender;
	}

}
