package br.com.fluentvalidator.model;

import java.util.List;
import java.util.UUID;

public class Parent {

    private String id;

    private String name;

    private Integer age;

    private List<String> cities;

    private List<Child> children;

    public Parent() {
        id = UUID.randomUUID().toString();
    }

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    public List<Child> getChildren() {
        return children;
    }

    public void setChildren(final List<Child> children) {
        this.children = children;
    }

    public List<String> getCities() {
        return cities;
    }

    public void setCities(final List<String> cities) {
        this.cities = cities;
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public Integer getAge() {
        return age;
    }

    public void setAge(final Integer age) {
        this.age = age;
    }

}
