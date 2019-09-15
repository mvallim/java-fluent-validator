# 3. Spring support

## 3.1 Use Spring IoC to create validators

### [ValidatorSpringChild](../src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringChild.java)

```java
@Component
public class ValidatorSpringChild extends AbstractValidator<Child>{

    @Override
    protected void rules() {

        setPropertyOnContext("child");

        ruleFor(Child::getAge)
            .must(not(nullValue()))
                .withMessage("child age must be not null")
                .withFieldName("age")
            .must(greaterThanOrEqual(5))
                .withMessage("child age must be greater than or equal to 5")
                .withFieldName("age")
            .must(this::checkAgeConstraintChild)
                .withMessage("child age must be less than age parent")
                .withFieldName("age")
                .critical();

        ruleFor(Child::getName)
            .must(not(stringEmptyOrNull()))
                .withMessage("child name must be not null or empty")
                .withFieldName("name");

    }

    private boolean checkAgeConstraintChild(final Integer age) {
        return age < getPropertyOnContext("parent", Parent.class).getAge();
    }

}
```

### [ValidationSpringGirl](../src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringGirl.java)

```java
@Component
public class ValidatorSpringGirl extends AbstractValidator<Girl> {

    @Override
    protected void rules() {

        ruleFor(Girl::getGender)
            .must(equalTo(Gender.FEMALE))
                .when(not(nullValue(Gender.class)))
                .withMessage("gender of girl must be FEMALE")
                .withFieldName("gender");

        ruleFor(Girl::getName)
            .must(stringContains("Ana"))
                .when(not(stringEmptyOrNull()))
                .withMessage("child name must contains key Ana")
                .withFieldName("name");
    }

}
```

### [ValidationSpringBoy](../src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringBoy.java)

```java
@Component
public class ValidatorSpringBoy extends AbstractValidator<Boy>{

    @Override
    protected void rules() {

        ruleFor(Boy::getGender)
            .must(equalTo(Gender.MALE))
                .when(not(nullValue(Gender.class)))
                .withMessage("gender of boy must be MALE")
                .withFieldName("gender")
                .critical();

        ruleFor(Boy::getName)
            .must(stringContains("John"))
                .when(not(stringEmptyOrNull()))
                .withMessage("child name must contains key John")
                .withFieldName("name");
    }

}
```

### [ValidatorSpringParent](../src/test/java/br/com/fluentvalidator/spring/validator/ValidatorSpringParent.java)

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
            .must(not(nullValue()))
                .withMessage("parent's children cannot be null")
                .withCode("555")
                .withFieldName("children")
            .must(not(empty()))
                .when(not(nullValue()))
                .withMessage("parent must have at least one child")
                .withFieldName("children")
            .whenever(not(nullValue()))
                .withValidator(validatorChild)
                .critical();

        ruleFor(Parent::getId)
            .whenever(isTrue())
                .withValidator(validatorId)
                .critical();

        ruleFor(Parent::getAge)
            .must(greaterThanOrEqual(5))
                .when(not(nullValue()))
                .withMessage("age must be greater than or equal to 10")
                .withFieldName("age")
            .must(lessThanOrEqual(7))
                .when(not(nullValue()))
                .withMessage("age must be less than or equal to 7")
                .withCode("666")
                .withFieldName("age");

        ruleFor(Parent::getCities)
            .must(hasSize(10))
                .when(not(nullValue()))
                .withMessage("cities size must be 10")
                .withFieldName("cities");

        ruleFor(Parent::getName)
            .must(stringContains("John"))
                .when(not(stringEmptyOrNull()))
                .withMessage("name must contains key John")
                .withFieldName("name");

        ruleForEach(parent -> extractGirls(parent.getChildren()))
            .whenever(not(nullValue()))
                .withValidator(validatorGirl);

        ruleForEach(parent -> extractBoys(parent.getChildren()))
            .whenever(not(nullValue()))
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

## 3.2 Execute validation and get results

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
