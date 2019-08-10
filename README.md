# Java Fluent Validator

[![Build Status](https://travis-ci.org/mvallim/java-fluent-validator.svg?branch=master)](https://travis-ci.org/mvallim/java-fluent-validator)
[![Coverage Status](https://coveralls.io/repos/github/mvallim/java-fluent-validator/badge.svg?branch=master)](https://coveralls.io/github/mvallim/java-fluent-validator?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/br.com.fluentvalidator/java-fluent-validator/badge.svg)](https://maven-badges.herokuapp.com/maven-central/br.com.fluentvalidator/java-fluent-validator)
[![Hex.pm](https://img.shields.io/hexpm/l/plug.svg)](http://www.apache.org/licenses/LICENSE-2.0)

> **(Documentation is under construction)**

Validating data is a common task that occurs throughout any application, especially the business logic layer. As for some quite complex scenarios, often the same or similar validations are scattered everywhere, thus it is hard to reuse code and break the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) rule.

## 1. Quick Start

This chapter will show you how to get started with Fluent Validator.

### 1.1 Prerequisite

In order to use Fluent Validator within a Maven project, simply add the following dependency to your pom.xml. There are no other dependencies for Fluent Validator, which means other unwanted libraries will not overwhelm your project.

```xml
<dependency>
    <groupId>br.com.fluentvalidator</groupId>
    <artifactId>java-fluent-validator</artifactId>
    <version>0.0.1</version>
</dependency>
```

### 1.2 Create a domain model

Create a domain model or you can call it entity to be validated on later. For example, a Parent and Child instance is created as below.

#### [Parent](src/test/java/br/com/fluentvalidator/model/Parent.java)

```java
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
```

#### [Child](src/test/java/br/com/fluentvalidator/model/Child.java)

```java
public class Child {

    private String name;

    private Integer age;

    public Child(final String name, int age) {
        this.name = name;
        this.age = age;
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
```

## 2. Basic validation step by step

Fluent Valiator is inspired by [Fluent Interface](https://www.martinfowler.com/bliki/FluentInterface.html) which defined an inner-DSL within Java language for programmers to use. A fluent interface implies that its primary goal is to make it easy to SPEAK and UNDERSTAND. And that is what FluentValiator is dedicated to do, to provide more readable code for you.

### 2.1 Create validators

#### [ValidatorChild](src/test/java/br/com/fluentvalidator/validator/ValidatorChild.java)

```java
public class ValidatorChild extends AbstractValidator<Child>{

    @Override
    protected void rules() {

        ruleFor(Child::getAge)
            .when(age -> true)
            .must(age -> notNullValue().matches(age))
            .withMessage("child age must be not null")
            .must(age -> greaterThanOrEqualTo(5).matches(age))
            .withMessage("child age must be greater than or equal to 5");

        ruleFor(Child::getName)
            .when(name -> true)
            .must(name -> not(isEmptyOrNullString()).matches(name))
            .withMessage("child name must be not null or empty")
            .must(name -> containsString("John").matches(name))
            .withMessage("child name must contains key John");

    }
}
```

#### [ValidationParent](src/test/java/br/com/fluentvalidator/validator/ValidationParent.java)

```java
public class ValidationParent extends AbstractValidator<Parent> {

    @Override
    protected void rules() {

        ruleFor(Parent::getAge)
            .when(age -> notNullValue().matches(age))
            .must(age -> greaterThanOrEqualTo(5).matches(age))
            .withMessage("age must be greater than or equal to 10")
            .must(age -> lessThanOrEqualTo(7).matches(age))
            .withMessage("age must be less than or equal to 7");

        ruleFor(Parent::getCities)
            .when(cities -> notNullValue().matches(cities))
            .must(cities -> hasSize(10).matches(cities))
            .withMessage("cities size must be 10");

        ruleFor(Parent::getName)
            .when(name -> not(isEmptyOrNullString()).matches(name))
            .must(name -> containsString("John").matches(name))
            .withMessage("name must contains key John");

        ruleFor(Parent::getChildren)
            .when(children -> true)
            .must(children -> notNullValue().matches(children))
            .withMessage("parent's children cannot be null")
            .must(children -> not(empty()).matches(children))
            .withMessage("parent must have at least one child");

        ruleForEach(Parent::getChildren)
            .when(children -> notNullValue().matches(children))
            .withValidator(new ValidatorChild());

    }
}
```

### 2.2 Validate on fields or instances

### 2.3 Execute validation and get results

```java
final AbstractValidator<Parent> validationParent = new ValidationParent();

final Parent parent = new Parent();

parent.setAge(10);
parent.setName("Ana");
parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
parent.setChildren(Arrays.asList(new Child("Ana", 4)));

final ValidationResult result = validationParent.validate(parent);

assertFalse(result.isValid());
assertThat(result.getErrors(), not(empty()));
assertThat(result.getErrors(), hasSize(5));
assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be greater than or equal to 5"))));
assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
```
