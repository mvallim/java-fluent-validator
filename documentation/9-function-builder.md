# 9. FunctionBuilder methods

This builder is null safe to access sequence of accessors. If any result is null the result is propagated at to the end of accessor.

```java
of(final Function<I, O> function)
```

Example:

```java
public class ComplexObject {

    private City city;

    public City getCity() {
        return this.city;
    }

}

public class City {

    private String name;

    private State state;

    public String getName() {
        return this.name;
    }

    public State getState() {
        return this.state;
    }

}

public class State {

    public String name;

    public String getName() {
        return this.name;
    }

}
```

```java

import static br.com.fluentvalidator.function.FunctionBuilder.of;

private static Predicate<ComplexObject> hasStateName() {
    return PredicateBuilder.from(not(nullValue()))
        .and(not(nullValue(of(ComplexObject::getCity).andThen(City::getState).andThen(State:getName))));
}
```
