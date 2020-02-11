# 1. Quick Start

This chapter will show you how to get started with Java Fluent Validator.

## 1.1 Prerequisite

In order to use Java Fluent Validator within a Maven project, simply add the following dependency to your pom.xml. There are no other dependencies for Java Fluent Validator, which means other unwanted libraries will not overwhelm your project.

You can pull it from the central Maven repositories:

```xml
<dependency>
    <groupId>com.github.mvallim</groupId>
    <artifactId>java-fluent-validator</artifactId>
    <version>1.7.8</version>
</dependency>
```

If you want to try a snapshot version, add the following repository:

```xml
<repository>
    <id>sonatype-snapshots</id>
    <name>Sonatype Snapshots</name>
    <url>https://oss.sonatype.org/content/repositories/snapshots</url>
    <snapshots>
        <enabled>true</enabled>
    </snapshots>
</repository>
```

## 1.2 Create a domain model

Create a domain model or you can call it entity to be validated on later. For example, a Parent, Child, Boy and Girl instance is created as below.

### [Parent](../src/test/java/br/com/fluentvalidator/model/Parent.java)

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

### [Child](../src/test/java/br/com/fluentvalidator/model/Child.java)

```java
public abstract class Child {

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

### [Girl](../src/test/java/br/com/fluentvalidator/model/Girl.java)

```java
public class Girl extends Child {

    private final Gender gender =  Gender.FEMALE;

    public Girl(String name, int age) {
        super(name, age);
    }

    public Gender getGender() {
        return this.gender;
    }

}
```

### [Boy](../src/test/java/br/com/fluentvalidator/model/Boy.java)

```java
public class Boy extends Child {

    private final Gender gender =  Gender.MALE;

    public Boy(String name, int age) {
        super(name, age);
    }

    public Gender getGender() {
        return this.gender;
    }

}
```
