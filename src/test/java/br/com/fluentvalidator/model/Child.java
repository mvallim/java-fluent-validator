package br.com.fluentvalidator.model;

public abstract class Child {

	private final String name;

	private final Integer age;

	public Child(final String name, int age) {
		this.name = name;
		this.age = age;
	}

	public String getName() {
		return this.name;
	}

	public Integer getAge() {
		return this.age;
	}

}
