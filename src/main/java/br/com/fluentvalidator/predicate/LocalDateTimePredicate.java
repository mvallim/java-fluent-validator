package br.com.fluentvalidator.predicate;

import java.time.LocalDateTime;
import java.util.function.Function;
import java.util.function.Predicate;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

public final class LocalDateTimePredicate {

  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeAfterNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateTime.isAfter(LocalDateTime.now()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfterNow(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> localDateTimeAfterNow().test(source.apply(obj)));
  }

  /**
   *
   * @param <T>
   * @return
   */
  public static <T extends LocalDateTime> Predicate<T> localDateTimeBeforeNow() {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(localDateTime -> localDateTime.isBefore(LocalDateTime.now()));
  }

  /**
   *
   * @param source
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBeforeNow(final Function<T, LocalDateTime> source) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> localDateTimeBeforeNow().test(source.apply(obj)));
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeAfter(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> {
          final LocalDateTime sourceDateTime = source.apply(obj);
          final LocalDateTime targetDateTime = target.apply(obj);
          return sourceDateTime.isAfter(targetDateTime);
        });
  }

  /**
   *
   * @param source
   * @param target
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBefore(final Function<T, LocalDateTime> source, final Function<T, LocalDateTime> target) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> {
          final LocalDateTime sourceDateTime = source.apply(obj);
          final LocalDateTime targetDateTime = target.apply(obj);
          return sourceDateTime.isBefore(targetDateTime);
        });
  }

  /**
   *
   * @param source
   * @param min
   * @param max
   * @param <T>
   * @return
   */
  public static <T> Predicate<T> localDateTimeBetween(final Function<T, LocalDateTime> source, final LocalDateTime min, final LocalDateTime max) {
    return PredicateBuilder.<T>from(not(nullValue()))
        .and(obj -> source.apply(obj).isAfter(min)).or(obj -> source.apply(obj).equals(min))
        .and(obj -> source.apply(obj).isBefore(max)).or(obj -> source.apply(obj).equals(max));
  }

  public LocalDateTimePredicate() {
    super();
  }

}
