<p align="center"><img src="documentation/fluent.png" height="20%" width="20%"></p>

# Java Fluent Validator

![Java CI with Maven](https://github.com/mvallim/java-fluent-validator/workflows/Java%20CI%20with%20Maven/badge.svg?branch=master)
![CodeQL](https://github.com/mvallim/java-fluent-validator/workflows/CodeQL/badge.svg?branch=master)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=java-fluent-validator&metric=alert_status)](https://sonarcloud.io/dashboard?id=java-fluent-validator)
[![Coverage](https://sonarcloud.io/api/project_badges/measure?project=java-fluent-validator&metric=coverage)](https://sonarcloud.io/dashboard?id=java-fluent-validator)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.mvallim/java-fluent-validator/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.mvallim/java-fluent-validator)
[![Hex.pm](https://img.shields.io/hexpm/l/plug.svg)](http://www.apache.org/licenses/LICENSE-2.0)

Validating data is a common task that occurs throughout any application, especially the business logic layer. As for some quite complex scenarios, often the same or similar validations are scattered everywhere, thus it is hard to reuse code and break the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) rule.

_**Compatible JDK 8, 11, 15, 16 and 17**_

This library supports **`Kotlin`** aswell

## Sample

### Java

```java
import static br.com.fluentvalidator.predicate.ComparablePredicate.equalTo;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.model.Boy
import br.com.fluentvalidator.model.Gender;

import br.com.fluentvalidator.AbstractValidator;

public class JavaValidatorBoy extends AbstractValidator<Boy> {

  @Override
  protected void rules() {

    ruleFor(Boy::getGender)
      .must(equalTo(Gender.MALE))
      .when(not(nullValue()))
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

### Kotlin

```kotlin
import br.com.fluentvalidator.predicate.ComparablePredicate.equalTo;
import br.com.fluentvalidator.predicate.LogicalPredicate.not;
import br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import br.com.fluentvalidator.predicate.StringPredicate.stringContains;
import br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import br.com.fluentvalidator.model.Boy
import br.com.fluentvalidator.model.Gender;

import br.com.fluentvalidator.AbstractValidator;

class KotlinValidatorBoy : AbstractValidator<Boy> {
	
  constructor() : super();

  override fun rules() {

    ruleFor(Boy::getGender)
      .must(equalTo(Gender.MALE))
      .`when`(not(nullValue()))
        .withMessage("gender of boy must be MALE")
        .withFieldName("gender")
        .critical();

    ruleFor(Boy::getName)
      .must(stringContains("John"))
      .`when`(not(stringEmptyOrNull()))
        .withMessage("child name must contains key John")
        .withFieldName("name");
  }

}
```

## Index

1. [Quick start](documentation/1-quick-start.md)
   - [Step by Step](documentation/2-step-by-step.md)
   - [Spring support](documentation/3-spring-support.md)
2. [Validator](documentation/4-validator-methods.md)
3. [Builder](documentation/5-builder-methods.md)
4. [Predicate](documentation/6-predicate-methods.md)
5. [ValidationResult](documentation/7-validation-methods.md)
6. [PredicateBuilder](documentation/8-predicate-builder.md)
7. [FunctionBuilder](documentation/9-function-builder.md)
8. Examples
   - [Samples](src/test/java/br/com/fluentvalidator/ValidatorTest.java)
   - [Spring support samples](src/test/java/br/com/fluentvalidator/spring/ValidatorSpringTest.java)

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [GitHub](https://github.com/mvallim/java-fluent-validator) for versioning. For the versions available, see the [tags on this repository](https://github.com/mvallim/java-fluent-validator/tags).

## Authors

* **Marcos Vallim** - *Founder, Author, Development, Test, Documentation* - [mvallim](https://github.com/mvallim)
* **Paulo Sergio** - *Manteiner, Development, Test, Documentation* - [paulosergio-jnr](https://github.com/paulosergio-jnr)

See also the list of [contributors](CONTRIBUTORS.txt) who participated in this project.

## License

This project is licensed under the Apache License - see the [LICENSE](LICENSE) file for details
