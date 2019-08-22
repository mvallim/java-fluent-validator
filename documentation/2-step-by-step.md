# 2. Basic validation step by step

Java Fluent Valiator is inspired by [Fluent Interface](https://www.martinfowler.com/bliki/FluentInterface.html) and [.Net FluentValidation](https://fluentvalidation.net/) which defined an inner-DSL within Java language for programmers to use. A fluent interface implies that its primary goal is to make it easy to SPEAK and UNDERSTAND. And that is what Java Fluent Valiator is dedicated to do, to provide more readable code for you.

## 2.1 Create validators

### [ValidatorChild](src/test/java/br/com/fluentvalidator/validator/ValidatorChild.java)

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

### [ValidationGirl](src/test/java/br/com/fluentvalidator/validator/ValidatorGirl.java)

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

### [ValidationBoy](src/test/java/br/com/fluentvalidator/validator/ValidatorBoy.java)

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

### [ValidatorParent](src/test/java/br/com/fluentvalidator/validator/ValidatorParent.java)

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

## 2.2 Execute validation and get results

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
