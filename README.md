# Java Fluent Validator

[![Build Status](https://travis-ci.org/mvallim/java-fluent-validator.svg?branch=master)](https://travis-ci.org/mvallim/java-fluent-validator)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=java-fluent-validator&metric=alert_status)](https://sonarcloud.io/dashboard?id=java-fluent-validator)
[![Coverage](https://sonarcloud.io/api/project_badges/measure?project=java-fluent-validator&metric=coverage)](https://sonarcloud.io/dashboard?id=java-fluent-validator)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.mvallim/java-fluent-validator/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.mvallim/java-fluent-validator)
[![Hex.pm](https://img.shields.io/hexpm/l/plug.svg)](http://www.apache.org/licenses/LICENSE-2.0)

> **(Documentation is under construction)**

Validating data is a common task that occurs throughout any application, especially the business logic layer. As for some quite complex scenarios, often the same or similar validations are scattered everywhere, thus it is hard to reuse code and break the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) rule.

## 1. Quick Start

This chapter will show you how to get started with Java Fluent Validator.

### 1.1 Prerequisite

In order to use Java Fluent Validator within a Maven project, simply add the following dependency to your pom.xml. There are no other dependencies for Java Fluent Validator, which means other unwanted libraries will not overwhelm your project.

You can pull it from the central Maven repositories:

```xml
<dependency>
    <groupId>com.github.mvallim</groupId>
    <artifactId>java-fluent-validator</artifactId>
    <version>0.0.6</version>
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

### 1.2 Create a domain model

Create a domain model or you can call it entity to be validated on later. For example, a Parent, Child, Boy and Girl instance is created as below.

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

#### [Girl](src/test/java/br/com/fluentvalidator/model/Girl.java)

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

#### [Boy](src/test/java/br/com/fluentvalidator/model/Boy.java)

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

## 2. Basic validation step by step

Java Fluent Valiator is inspired by [Fluent Interface](https://www.martinfowler.com/bliki/FluentInterface.html) and [.Net FluentValidation](https://fluentvalidation.net/) which defined an inner-DSL within Java language for programmers to use. A fluent interface implies that its primary goal is to make it easy to SPEAK and UNDERSTAND. And that is what Java Fluent Valiator is dedicated to do, to provide more readable code for you.

### 2.1 Create validators

#### [ValidatorChild](src/test/java/br/com/fluentvalidator/validator/ValidatorChild.java)

```java
public class ValidatorChild extends AbstractValidator<Child>{

    @Override
    protected void rules() {

        setPropertyOnContext("child");

        ruleFor(Child::getAge)
            .when(isTrue())
                .must(not(nullValue()))
                .withMessage("child age must be not null")
                .withFieldName("age")
            .when(isTrue())
                .must(greaterThanOrEqual(5))
                .withMessage("child age must be greater than or equal to 5")
                .withFieldName("age")
            .when(isTrue())
                .must(this::checkAgeConstraintChild)
                .withMessage("child age must be less than age parent")
                .withFieldName("age")
                .critical();

        ruleFor(Child::getName)
            .when(isTrue())
                .must(not(stringEmptyOrNull()))
                .withMessage("child name must be not null or empty")
                .withFieldName("name");

    }

    private boolean checkAgeConstraintChild(final Integer age) {
        return age < getPropertyOnContext("parent", Parent.class).getAge();
    }

}
```

#### [ValidationGirl](src/test/java/br/com/fluentvalidator/validator/ValidatorGirl.java)

```java
public class ValidatorGirl extends AbstractValidator<Girl>{

    @Override
    protected void rules() {

        ruleFor(Girl::getGender)
            .when(not(nullValue(Gender.class)))
                .must(equalTo(Gender.FEMALE))
                .withMessage("gender of girl must be FEMALE")
                .withFieldName("gender");

        ruleFor(Girl::getName)
            .when(not(stringEmptyOrNull()))
                .must(stringContains("Ana"))
                .withMessage("child name must contains key Ana")
                .withFieldName("name");
    }

}
```

#### [ValidationBoy](src/test/java/br/com/fluentvalidator/validator/ValidatorBoy.java)

```java
public class ValidatorBoy extends AbstractValidator<Boy>{

    @Override
    protected void rules() {

        ruleFor(Boy::getGender)
            .when(not(nullValue(Gender.class)))
                .must(equalTo(Gender.MALE))
                .withMessage("gender of boy must be MALE")
                .withFieldName("gender")
                .critical();

        ruleFor(Boy::getName)
            .when(not(stringEmptyOrNull()))
                .must(stringContains("John"))
                .withMessage("child name must contains key John")
                .withFieldName("name");
    }

}
```

#### [ValidatorParent](src/test/java/br/com/fluentvalidator/validator/ValidatorParent.java)

```java
public class ValidatorParent extends AbstractValidator<Parent>{

    @Override
    protected void rules() {

        setPropertyOnContext("parent");

        ruleForEach(Parent::getChildren)
            .when(isTrue())
                .must(not(nullValue()))
                .withMessage("parent's children cannot be null")
                .withCode("555")
                .withFieldName("children")
            .when(not(nullValue()))
                .must(not(empty()))
                .withMessage("parent must have at least one child")
                .withFieldName("children")
            .when(not(nullValue()))
                .withValidator(new ValidatorChild())
                .critical();

        ruleFor(Parent::getId)
            .when(isTrue())
                .withValidator(new ValidatorId())
                .critical();

        ruleFor(Parent::getAge)
            .when(not(nullValue()))
                .must(greaterThanOrEqual(5))
                .withMessage("age must be greater than or equal to 10")
                .withFieldName("age")
            .when(not(nullValue()))
                .must(lessThanOrEqual(7))
                .withMessage("age must be less than or equal to 7")
                .withCode("666")
                .withFieldName("age");

        ruleFor(Parent::getCities)
            .when(not(nullValue()))
                .must(hasSize(10))
                .withMessage("cities size must be 10")
                .withFieldName("cities");

        ruleFor(Parent::getName)
            .when(not(stringEmptyOrNull()))
                .must(stringContains("John"))
                .withMessage("name must contains key John")
                .withFieldName("name");

        ruleForEach(parent -> extractGirls(parent.getChildren()))
            .when(not(nullValue()))
                .withValidator(new ValidatorGirl());

        ruleForEach(parent -> extractBoys(parent.getChildren()))
            .when(not(nullValue()))
                .withValidator(new ValidatorBoy())
                .critical();

    }

    private Collection<Girl> extractGirls(Collection<Child> children) {
        return Optional.ofNullable(children).orElseGet(ArrayList::new)
                .stream()
                .filter(Girl.class::isInstance)
                .map(Girl.class::cast)
                .collect(Collectors.toList());
    }

    private Collection<Boy> extractBoys(Collection<Child> children) {
        return Optional.ofNullable(children).orElseGet(ArrayList::new)
                .stream()
                .filter(Boy.class::isInstance)
                .map(Boy.class::cast)
                .collect(Collectors.toList());
    }

}
```

### 2.2 Execute validation and get results

```java
public class ValidatorTest {

    @Test
    public void validationMustBeFalseWhenParentAndChildrenIsInvalid() {

        final Validator<Parent> validatorParent = new ValidatorParent();

        final Parent parent = new Parent();

        parent.setAge(10);
        parent.setName("Ana");
        parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
        parent.setChildren(Arrays.asList(new Girl("Barbara", 4)));

        final ValidationResult result = validatorParent.validate(parent);

        assertFalse(result.isValid());
        assertThat(result.getErrors(), not(empty()));
        assertThat(result.getErrors(), hasSize(5));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getAge()))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("666"))));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(4))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be greater than or equal to 5"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Barbara"))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child name must contains key Ana"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));
    }

}
```

## 3. Spring support

### 3.1 Use Spring IoC to create validators

#### [ValidatorSpringChild](src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringChild.java)

```java
@Component
public class ValidatorSpringChild extends AbstractValidator<Child>{

    @Override
    protected void rules() {

        setPropertyOnContext("child");

        ruleFor(Child::getAge)
            .when(isTrue())
                .must(not(nullValue()))
                .withMessage("child age must be not null")
                .withFieldName("age")
            .when(isTrue())
                .must(greaterThanOrEqual(5))
                .withMessage("child age must be greater than or equal to 5")
                .withFieldName("age")
            .when(isTrue())
                .must(this::checkAgeConstraintChild)
                .withMessage("child age must be less than age parent")
                .withFieldName("age")
                .critical();

        ruleFor(Child::getName)
            .when(isTrue())
                .must(not(stringEmptyOrNull()))
                .withMessage("child name must be not null or empty")
                .withFieldName("name");

    }

    private boolean checkAgeConstraintChild(final Integer age) {
        return age < getPropertyOnContext("parent", Parent.class).getAge();
    }

}
```

#### [ValidationSpringGirl](src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringGirl.java)

```java
@Component
public class ValidatorSpringGirl extends AbstractValidator<Girl> {

    @Override
    protected void rules() {

        ruleFor(Girl::getGender)
            .when(not(nullValue(Gender.class)))
                .must(equalTo(Gender.FEMALE))
                .withMessage("gender of girl must be FEMALE")
                .withFieldName("gender");

        ruleFor(Girl::getName)
            .when(not(stringEmptyOrNull()))
                .must(stringContains("Ana"))
                .withMessage("child name must contains key Ana")
                .withFieldName("name");
    }

}
```

#### [ValidationSpringBoy](src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringBoy.java)

```java
@Component
public class ValidatorSpringBoy extends AbstractValidator<Boy>{

    @Override
    protected void rules() {

        ruleFor(Boy::getGender)
            .when(not(nullValue(Gender.class)))
                .must(equalTo(Gender.MALE))
                .withMessage("gender of boy must be MALE")
                .withFieldName("gender")
                .critical();

        ruleFor(Boy::getName)
            .when(not(stringEmptyOrNull()))
                .must(stringContains("John"))
                .withMessage("child name must contains key John")
                .withFieldName("name");
    }

}
```

#### [ValidatorSpringParent](src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringParent.java)

```java
@Component
public class ValidatorSpringParent extends AbstractValidator<Parent> {

    @Autowired
    ValidatorSpringChild validatorChild;

    @Autowired
    ValidatorSpringId validatorId;

    @Autowired
    ValidatorSpringGirl validatorGirl;

    @Autowired
    ValidatorSpringBoy validatorBoy;

    @Override
    protected void rules() {

        setPropertyOnContext("parent");

        ruleForEach(Parent::getChildren)
            .when(isTrue())
                .must(not(nullValue()))
                .withMessage("parent's children cannot be null")
                .withCode("555")
                .withFieldName("children")
            .when(not(nullValue()))
                .must(not(empty()))
                .withMessage("parent must have at least one child")
                .withFieldName("children")
            .when(not(nullValue()))
                .withValidator(validatorChild)
                .critical();

        ruleFor(Parent::getId)
            .when(isTrue())
                .withValidator(validatorId)
                .critical();

        ruleFor(Parent::getAge)
            .when(not(nullValue()))
                .must(greaterThanOrEqual(5))
                .withMessage("age must be greater than or equal to 10")
                .withFieldName("age")
            .when(not(nullValue()))
                .must(lessThanOrEqual(7))
                .withMessage("age must be less than or equal to 7")
                .withCode("666")
                .withFieldName("age");

        ruleFor(Parent::getCities)
            .when(not(nullValue()))
                .must(hasSize(10))
                .withMessage("cities size must be 10")
                .withFieldName("cities");

        ruleFor(Parent::getName)
            .when(not(stringEmptyOrNull()))
                .must(stringContains("John"))
                .withMessage("name must contains key John")
                .withFieldName("name");

        ruleForEach(parent -> extractGirls(parent.getChildren()))
            .when(not(nullValue()))
                .withValidator(validatorGirl);

        ruleForEach(parent -> extractBoys(parent.getChildren()))
            .when(not(nullValue()))
                .withValidator(validatorBoy)
                .critical();

    }

    private Collection<Girl> extractGirls(Collection<Child> children) {
        return Optional.ofNullable(children).orElseGet(ArrayList::new)
                .stream()
                .filter(Girl.class::isInstance)
                .map(Girl.class::cast)
                .collect(Collectors.toList());
    }

    private Collection<Boy> extractBoys(Collection<Child> children) {
        return Optional.ofNullable(children).orElseGet(ArrayList::new)
                .stream()
                .filter(Boy.class::isInstance)
                .map(Boy.class::cast)
                .collect(Collectors.toList());
    }

}
```

### 3.2 Execute validation and get results

```java
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = ValidatorSpringConfig.class)
public class ValidatorSpringTest {

    @Autowired
    ValidatorSpringParent validatorParent;

    @Test
    public void validationMustBeFalseWhenParentAndChildrenIsInvalid() {
        final Parent parent = new Parent();

        parent.setAge(10);
        parent.setName("Ana");
        parent.setCities(Arrays.asList("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"));
        parent.setChildren(Arrays.asList(new Girl("Barbara", 4)));

        final ValidationResult result = validatorParent.validate(parent);

        assertFalse(result.isValid());
        assertThat(result.getErrors(), not(empty()));
        assertThat(result.getErrors(), hasSize(5));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Ana"))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("name must contains key John"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getAge()))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("age must be less than or equal to 7"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", containsString("666"))));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("cities"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(parent.getCities()))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("cities size must be 10"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("age"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", equalTo(4))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child age must be greater than or equal to 5"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));

        assertThat(result.getErrors(), hasItem(hasProperty("field", containsString("name"))));
        assertThat(result.getErrors(), hasItem(hasProperty("attemptedValue", containsString("Barbara"))));
        assertThat(result.getErrors(), hasItem(hasProperty("message", containsString("child name must contains key Ana"))));
        assertThat(result.getErrors(), hasItem(hasProperty("code", nullValue())));
    }

}
```

## 4. Validator methods

### 4.1 validate instance

* `validate(final T instance)`

### 4.2 validate collection

* `validate(final Collection<T> instances)`

### 4.3 ruleFor instance

* `ruleFor(final Function<T, P> function)`

### 4.3 ruleForEach collection

* `ruleForEach(final Function<T, Collection<P>> function)`

## 5. Chain methods

### 5.1 when condition to validate

* `when(final Predicate<T> predicate)`

### 5.2 must condition to valid

* `must(final Predicate<T> predicate)`

### 5.3 withMessage when `must` condition not be true

* `withMessage(final String message)`

### 5.4 withCode when `must` condition not be true

* `withCode(final String code)`

### 5.5 withFieldName when `must` condition not be true

* `withFieldName(final String fieldName)`

### 5.6 withValidator to validate object

* `withValidator(final Validator<P> validator)`

### 5.7 critical path in chain validation

* `critical()`

## 6. Predicates

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

Represents a predicate (boolean-valued function) of one argument.

### 6.1 Logical

* `not(final Predicate<T> predicate)`
* `isTrue()`
* `isFalse()`

### 6.2 Object

* `nullValue()`
* `nullValue(final Class<T> clazz)`
* `equalTo(final T obj)`
* `instanceOf(final Class<?> clazz)`

### 6.3 String

* `stringSizeGreaterThan(final int size)`
* `stringSizeLessThan(final int size)`
* `stringSizeGreaterThanOrEqual(final int size)`
* `stringSizeLessThanOrEqual(final int size)`
* `stringSizeBetween(final int minSize, final int maxSize)`
* `stringEmptyOrNull()`
* `stringContains(final String str)`
* `matches(final String regex)`

### 6.4 Comparable

* `lessThan(final T max)`
* `greaterThan(final T min)`
* `greaterThanOrEqual(final T min)`
* `lessThanOrEqual(final T max)`
* `between(final T min, final T max)`

### 6.5 Collection

* `empty()`
* `hasItem(final Object object)`
* `hasItems(final Collection<Object> objects)`
* `hasItems(final Object... objects)`
* `hasAny(final Collection<Object> objects)`
* `hasAny(final Object... objects)`
* `hasSize(final int size)`

## 7. Examples

All test cases or samples can be found from the below links:

* [Samples](src/test/java/br/com/fluentvalidator/ValidatorTest.java)
* [Spring support samples](src/test/java/br/com/fluentvalidator/spring/ValidatorSpringTest.java)

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [GitHub](https://github.com/mvallim/kubernetes-under-the-hood) for versioning. For the versions available, see the [tags on this repository](https://github.com/mvallim/kubernetes-under-the-hood/tags). 

## Authors

* **Marcos Vallim** - *Initial work, Development, Test, Documentation* - [mvallim](https://github.com/mvallim)

See also the list of [contributors](CONTRIBUTORS.txt) who participated in this project.

## License

This project is licensed under the Apache License - see the [LICENSE](LICENSE) file for details
