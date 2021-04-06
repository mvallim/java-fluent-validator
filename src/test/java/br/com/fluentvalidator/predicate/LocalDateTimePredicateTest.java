package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeAfter;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeAfterNow;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeAfterOrEqual;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeAfterOrEqualToday;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeAfterToday;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBefore;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBeforeNow;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBeforeOrEqual;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBeforeOrEqualToday;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBeforeToday;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBetween;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeBetweenOrEqual;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeEqualTo;
import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.localDateTimeIsToday;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.junit.Test;

public class LocalDateTimePredicateTest {

  // region localDateTimeAfterToday

  @Test
  public void testLocalDateTimeAfterToday() {
    assertFalse(localDateTimeAfterToday().test(LocalDateTime.now()));
    assertTrue(localDateTimeAfterToday().test(LocalDateTime.now().plusDays(1)));
    assertFalse(localDateTimeAfterToday().test(LocalDateTime.now().minusDays(1)));

    assertFalse(localDateTimeAfterToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertTrue(localDateTimeAfterToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().plusDays(1), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeAfterToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().minusDays(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeAfterTodayNullValues() {
    assertFalse(localDateTimeAfterToday().test(null));

    assertFalse(localDateTimeAfterToday(null).test(null));
    assertFalse(localDateTimeAfterToday(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeAfterToday(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeAfterToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateAfterOrEqualToday

  @Test
  public void testLocalDateTimeAfterOrEqualToday() {
    assertTrue(localDateTimeAfterOrEqualToday().test(LocalDateTime.now()));
    assertTrue(localDateTimeAfterOrEqualToday().test(LocalDateTime.now().plusDays(1)));
    assertFalse(localDateTimeAfterOrEqualToday().test(LocalDateTime.now().minusDays(1)));

    assertTrue(localDateTimeAfterOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertTrue(localDateTimeAfterOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().plusDays(1), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeAfterOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().minusDays(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeAfterOrEqualTodayNullValues() {
    assertFalse(localDateTimeAfterOrEqualToday().test(null));

    assertFalse(localDateTimeAfterOrEqualToday(null).test(null));
    assertFalse(localDateTimeAfterOrEqualToday(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeAfterOrEqualToday(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeAfterOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateTimeBeforeToday

  @Test
  public void testLocalDateTimeBeforeToday() {
    assertFalse(localDateTimeBeforeToday().test(LocalDateTime.now()));
    assertFalse(localDateTimeBeforeToday().test(LocalDateTime.now().plusDays(1)));
    assertTrue(localDateTimeBeforeToday().test(LocalDateTime.now().minusDays(1)));

    assertFalse(localDateTimeBeforeToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeBeforeToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().plusDays(1), LocalDateTime.now(), LocalDateTime.now())));
    assertTrue(localDateTimeBeforeToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().minusDays(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeBeforeTodayNullValues() {
    assertFalse(localDateTimeBeforeToday().test(null));

    assertFalse(localDateTimeBeforeToday(null).test(null));
    assertFalse(localDateTimeBeforeToday(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeBeforeToday(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeBeforeToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateBeforeOrEqualToday

  @Test
  public void testLocalDateTimeBeforeOrEqualToday() {
    assertTrue(localDateTimeBeforeOrEqualToday().test(LocalDateTime.now()));
    assertFalse(localDateTimeBeforeOrEqualToday().test(LocalDateTime.now().plusDays(1)));
    assertTrue(localDateTimeBeforeOrEqualToday().test(LocalDateTime.now().minusDays(1)));

    assertTrue(localDateTimeBeforeOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeBeforeOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().plusDays(1), LocalDateTime.now(), LocalDateTime.now())));
    assertTrue(localDateTimeBeforeOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().minusDays(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeBeforeOrEqualTodayNullValues() {
    assertFalse(localDateTimeBeforeOrEqualToday().test(null));

    assertFalse(localDateTimeBeforeOrEqualToday(null).test(null));
    assertFalse(localDateTimeBeforeOrEqualToday(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeBeforeOrEqualToday(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeBeforeOrEqualToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateIsEqualToday

  @Test
  public void testLocalDateTimeIsToday() {
    assertTrue(localDateTimeIsToday().test(LocalDateTime.now()));
    assertFalse(localDateTimeIsToday().test(LocalDateTime.now().plusDays(1)));
    assertFalse(localDateTimeIsToday().test(LocalDateTime.now().minusDays(1)));

    assertTrue(localDateTimeIsToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeIsToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().plusDays(1), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeIsToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().minusDays(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeIsTodayNullValues() {
    assertFalse(localDateTimeBeforeOrEqualToday().test(null));

    assertFalse(localDateTimeIsToday(null).test(null));
    assertFalse(localDateTimeIsToday(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeIsToday(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeIsToday(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateIsEqualToday

  @Test
  public void testLocalDateTimeEqualTo() {
    final LocalDateTime now = LocalDateTime.now();

    assertTrue(localDateTimeEqualTo(now).test(now));
    assertFalse(localDateTimeEqualTo(now).test(now.plusDays(1)));
    assertFalse(localDateTimeEqualTo(now).test(now.minusDays(1)));

    assertTrue(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, now).test(new ObjectFromLocalDateTime(now, LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, now).test(new ObjectFromLocalDateTime(now.plusDays(1), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, now).test(new ObjectFromLocalDateTime(now.minusDays(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeEqualToNullValues() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeEqualTo(now).test(null));

    assertFalse(localDateTimeEqualTo(null).test(null));

    assertFalse(localDateTimeEqualTo(null, now).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, now).test(null));
    assertFalse(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, now).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));

    assertFalse(localDateTimeEqualTo(null, null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, null).test(null));
    assertFalse(localDateTimeEqualTo(ObjectFromLocalDateTime::getSource, null).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateTimeAfterNow

  @Test
  public void testLocalDateTimeAfterNow() {
    assertTrue(localDateTimeAfterNow().test(LocalDateTime.now().plusSeconds(1)));
    assertFalse(localDateTimeAfterNow().test(LocalDateTime.now().minusSeconds(1)));

    assertFalse(localDateTimeAfterNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertTrue(localDateTimeAfterNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().plusSeconds(1), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeAfterNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(LocalDateTime.now().minusSeconds(1), LocalDateTime.now(), LocalDateTime.now())));
  }

  @Test
  public void testLocalDateTimeAfterNowNullValues() {
    assertFalse(localDateTimeAfterNow().test(null));

    assertFalse(localDateTimeAfterNow(null).test(null));
    assertFalse(localDateTimeAfterNow(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeAfterNow(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeAfterNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateTimeBeforeNow

  @Test
  public void testLocalDateTimeBeforeNow() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeBeforeNow().test(now.plusSeconds(1)));
    assertTrue(localDateTimeBeforeNow().test(now.minusSeconds(1)));

    assertFalse(localDateTimeBeforeNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(now.plusSeconds(1), now, now)));
    assertTrue(localDateTimeBeforeNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(now.minusSeconds(1), now, now)));
  }

  @Test
  public void testLocalDateTimeBeforeNowNullValues() {
    assertFalse(localDateTimeBeforeNow().test(null));

    assertFalse(localDateTimeBeforeNow(null).test(null));
    assertFalse(localDateTimeBeforeNow(null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
    assertFalse(localDateTimeBeforeNow(ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeBeforeNow(ObjectFromLocalDateTime::getSource).test(new ObjectFromLocalDateTime(null, LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateAfter

  @Test
  public void testLocalDateTimeAfter() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeAfter(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeAfter(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.plusNanos(1), now)));
    assertTrue(localDateTimeAfter(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalDateTimeAfterNullValues() {
    assertFalse(localDateTimeAfter(null).test(null));
    assertFalse(localDateTimeAfter(null).test(LocalDateTime.now()));
    assertFalse(localDateTimeAfter(LocalDateTime.now()).test(null));

    assertFalse(localDateTimeAfter(null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeAfter(ObjectFromLocalDateTime::getSource, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeAfter(null, LocalDateTime.now()).test(null));
    assertFalse(localDateTimeAfter(null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));

    assertFalse(localDateTimeAfter(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeAfter(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeAfter(null, ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeAfter(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateTimeAfterOrEqual

  @Test
  public void testLocalDateTimeAfterOrEqual() {
    final LocalDateTime now = LocalDateTime.now();

    assertTrue(localDateTimeAfterOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeAfterOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.plusNanos(1), now)));
    assertTrue(localDateTimeAfterOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalDateTimeAfterOrEqualNullValues() {
    assertFalse(localDateTimeAfterOrEqual(null).test(null));
    assertFalse(localDateTimeAfterOrEqual(null).test(LocalDateTime.now()));
    assertFalse(localDateTimeAfterOrEqual(LocalDateTime.now()).test(null));

    assertFalse(localDateTimeAfterOrEqual(null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeAfterOrEqual(ObjectFromLocalDateTime::getSource, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeAfterOrEqual(null, LocalDateTime.now()).test(null));
    assertFalse(localDateTimeAfterOrEqual(null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));

    assertFalse(localDateTimeAfterOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeAfterOrEqual(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeAfterOrEqual(null, ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeAfterOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateBefore

  @Test
  public void testLocalDateTimeBefore() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeBefore(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBefore(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.plusNanos(1), now)));
    assertFalse(localDateTimeBefore(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalDateTimeBeforeNullValues() {
    assertFalse(localDateTimeBefore(null).test(null));
    assertFalse(localDateTimeBefore(null).test(LocalDateTime.now()));
    assertFalse(localDateTimeBefore(LocalDateTime.now()).test(null));

    assertFalse(localDateTimeBefore(null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBefore(ObjectFromLocalDateTime::getSource, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBefore(null, LocalDateTime.now()).test(null));
    assertFalse(localDateTimeBefore(null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));

    assertFalse(localDateTimeBefore(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBefore(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBefore(null, ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeBefore(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateTimeBeforeOrEqual

  @Test
  public void testLocalDateTimeBeforeOrEqual() {
    final LocalDateTime now = LocalDateTime.now();

    assertTrue(localDateTimeBeforeOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBeforeOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.plusNanos(1), now)));
    assertFalse(localDateTimeBeforeOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalDateTimeBeforeOrEqualNullValues() {
    assertFalse(localDateTimeBeforeOrEqual(null).test(null));
    assertFalse(localDateTimeBeforeOrEqual(null).test(LocalDateTime.now()));
    assertFalse(localDateTimeBeforeOrEqual(LocalDateTime.now()).test(null));

    assertFalse(localDateTimeBeforeOrEqual(null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBeforeOrEqual(ObjectFromLocalDateTime::getSource, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBeforeOrEqual(null, LocalDateTime.now()).test(null));
    assertFalse(localDateTimeBeforeOrEqual(null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));

    assertFalse(localDateTimeBeforeOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBeforeOrEqual(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBeforeOrEqual(null, ObjectFromLocalDateTime::getSource).test(null));
    assertFalse(localDateTimeBeforeOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(LocalDateTime.now(), LocalDateTime.now(), LocalDateTime.now())));
  }

  // endregion

  // region localDateBetween

  @Test
  public void testLocalDateTimeBetween() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeBetween(now, now).test(now));
    assertFalse(localDateTimeBetween(now.minusNanos(1), now).test(now));
    assertFalse(localDateTimeBetween(now, now.plusNanos(1)).test(now));
    assertTrue(localDateTimeBetween(now.minusNanos(1), now.plusNanos(1)).test(now));
    assertFalse(localDateTimeBetween(now.minusNanos(1), now.plusNanos(1)).test(now.plusNanos(2)));
    assertFalse(localDateTimeBetween(now.minusNanos(1), now.plusNanos(1)).test(now.minusNanos(2)));

    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now, now).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.plusNanos(2), now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.minusNanos(2), now, now)));

    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.plusNanos(2), now.minusNanos(1), now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.minusNanos(2), now.minusNanos(1), now)));

    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now.plusNanos(1))));
    assertTrue(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now.plusNanos(1))));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.plusNanos(2), now, now.plusNanos(1))));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.minusNanos(2), now, now.plusNanos(1))));

    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now.plusNanos(1))));
    assertTrue(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.plusNanos(2), now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.minusNanos(2), now.minusNanos(1), now.plusNanos(1))));

  }

  @Test
  public void testLocalDateTimeBetweenNullValues() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeBetween(null, null).test(null));
    assertFalse(localDateTimeBetween(now, null).test(now));
    assertFalse(localDateTimeBetween(null, now).test(now));
    assertFalse(localDateTimeBetween(now, now).test(null));

    assertFalse(localDateTimeBetween((Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween((Function<ObjectFromLocalDateTime, LocalDateTime>) null, now, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween((Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null, now).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, now).test(null));
    assertFalse(localDateTimeBetween((Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(now, now, now)));

    assertFalse(localDateTimeBetween(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween(null, ObjectFromLocalDateTime::getMin, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, now).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, now).test(null));
    assertFalse(localDateTimeBetween(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(now, now, now)));

    assertFalse(localDateTimeBetween(null, (LocalDateTime) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(null, now, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(null, (LocalDateTime) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, now, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetween(null, (LocalDateTime) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(now, now, now)));

    assertFalse(localDateTimeBetween(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(null, ObjectFromLocalDateTime::getMin, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetween(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetween(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(now, now, now)));

  }

  // endregion

  // region localDateBetweenOrEqual

  @Test
  public void testLocalDateTimeBetweenOrEqual() {
    final LocalDateTime now = LocalDateTime.now();

    assertTrue(localDateTimeBetweenOrEqual(now, now).test(now));
    assertTrue(localDateTimeBetweenOrEqual(now.minusNanos(1), now).test(now));
    assertTrue(localDateTimeBetweenOrEqual(now, now.plusNanos(1)).test(now));
    assertTrue(localDateTimeBetweenOrEqual(now.minusNanos(1), now.plusNanos(1)).test(now));
    assertFalse(localDateTimeBetweenOrEqual(now.minusNanos(1), now.plusNanos(1)).test(now.plusNanos(2)));
    assertFalse(localDateTimeBetweenOrEqual(now.minusNanos(1), now.plusNanos(1)).test(now.minusNanos(2)));

    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now, now).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now, now)));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.plusNanos(2), now, now)));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.minusNanos(2), now, now)));

    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.plusNanos(2), now.minusNanos(1), now)));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalDateTime(now.minusNanos(2), now.minusNanos(1), now)));

    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now.plusNanos(1))));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now.plusNanos(1))));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.plusNanos(2), now, now.plusNanos(1))));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now.minusNanos(1), ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.minusNanos(2), now, now.plusNanos(1))));

    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now)));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now, now.plusNanos(1))));
    assertTrue(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.plusNanos(2), now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now.minusNanos(2), now.minusNanos(1), now.plusNanos(1))));

  }

  @Test
  public void testLocalDateTimeBetweenOrEqualNullValues() {
    final LocalDateTime now = LocalDateTime.now();

    assertFalse(localDateTimeBetweenOrEqual(null, null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(now, null).test(now));
    assertFalse(localDateTimeBetweenOrEqual(null, now).test(now));
    assertFalse(localDateTimeBetweenOrEqual(now, now).test(null));

    assertFalse(localDateTimeBetweenOrEqual((Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual((Function<ObjectFromLocalDateTime, LocalDateTime>) null, now, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual((Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null, now).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, now).test(null));
    assertFalse(localDateTimeBetweenOrEqual((Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(now, now, now)));

    assertFalse(localDateTimeBetweenOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, ObjectFromLocalDateTime::getMin, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, now).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, (LocalDateTime) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, now).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (LocalDateTime) null).test(new ObjectFromLocalDateTime(now, now, now)));

    assertFalse(localDateTimeBetweenOrEqual(null, (LocalDateTime) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, now, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, (LocalDateTime) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, now, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (LocalDateTime) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, (LocalDateTime) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(now, now, now)));

    assertFalse(localDateTimeBetweenOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, ObjectFromLocalDateTime::getMin, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(null));
    assertFalse(localDateTimeBetweenOrEqual(ObjectFromLocalDateTime::getSource, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, ObjectFromLocalDateTime::getMax).test(null));
    assertFalse(localDateTimeBetweenOrEqual(null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null, (Function<ObjectFromLocalDateTime, LocalDateTime>) null).test(new ObjectFromLocalDateTime(now, now, now)));

  }

  // endregion

  // region multi thread test

  @Test
  public void testLocalDatePredicateMultiThreadMustBeTrue() throws InterruptedException {
    final int CONCURRENT_RUNNABLE = 100_000;
    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();
    final ExecutorService executorService = Executors.newFixedThreadPool(10);
    final LocalDateTime now = LocalDateTime.now();

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          assertThatCode(() -> {
            resultsOne.add(localDateTimeBetween(ObjectFromLocalDateTime::getSource, ObjectFromLocalDateTime::getMin, ObjectFromLocalDateTime::getMax).test(new ObjectFromLocalDateTime(now, now.minusNanos(1), now.plusNanos(1))));
          }).doesNotThrowAnyException();
        }
      });
    }

    executorService.shutdown();

    executorService.awaitTermination(10, TimeUnit.MINUTES);

    assertThat(resultsOne, hasSize(CONCURRENT_RUNNABLE));

    for (final Boolean result : resultsOne) {
      assertTrue(result);
    }
  }

  // endregion

  private static class ObjectFromLocalDateTime extends ObjectFrom<LocalDateTime> {

    private final LocalDateTime min;

    private final LocalDateTime max;

    public ObjectFromLocalDateTime(final LocalDateTime source, final LocalDateTime min, final LocalDateTime max) {
      super(source, null);
      this.min = min;
      this.max = max;
    }

    public LocalDateTime getMin() {
      return min;
    }

    public LocalDateTime getMax() {
      return max;
    }

  }

}
