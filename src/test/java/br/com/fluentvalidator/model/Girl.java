package br.com.fluentvalidator.model;

public class Girl extends Child {

	private final Gender gender =  Gender.FEMALE;

	public Girl(String name, int age) {
		super(name, age);
	}

	public Gender getGender() {
		return this.gender;
	}

}
