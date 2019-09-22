package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;

import java.util.Objects;
import java.util.function.Predicate;

public final class ObjectPredicate {

    private ObjectPredicate() {
        super();
    }

    /**
     *
     * @return
     */
    public static <T> Predicate<T> nullValue() {
        return PredicateBuilder.<T>from(Objects::isNull);
    }

    /**
     *
     * @param obj
     * @return
     */
    public static <T> Predicate<T> equalTo(final T obj) {
        return PredicateBuilder.<T>from(not(nullValue())).and(equalTo -> equalTo.equals(obj));
    }

    /**
     *
     * @param clazz
     * @return
     */
    public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
        return PredicateBuilder.<T>from(not(nullValue())).and(instanceOf -> not(nullValue()).test(clazz)).and(instanceOf -> clazz.isInstance(instanceOf));
    }

}
