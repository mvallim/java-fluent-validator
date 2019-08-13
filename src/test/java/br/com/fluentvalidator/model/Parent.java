package br.com.fluentvalidator.model;

import java.util.List;

public class Parent {

	private String name;

	private Integer age;

	private List<String> cities;

	private List<Child> children;

	public List<Child> getChildren() {
		return children;
	}

	public void setChildren(List<Child> children) {
		this.children = children;
	}

	public List<String> getCities() {
		return this.cities;
	}

	public void setCities(List<String> cities) {
		this.cities = cities;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Integer getAge() {
		return this.age;
	}

	public void setAge(Integer age) {
		this.age = age;
	}

}
