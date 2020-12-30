package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeAfter;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeAfterNow;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeAfterOrEqual;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeBefore;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeBeforeNow;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeBeforeOrEqual;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeBetween;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeBetweenOrEqual;
import static br.com.fluentvalidator.predicate.LocalTimePredicate.localTimeEqualTo;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalTime;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.junit.Test;

public class LocalTimePredicateTest {

  // region localDateIsEqualToday

  @Test
  public void testLocalTimeEqualTo() {
    final LocalTime now = LocalTime.now();

    assertTrue(localTimeEqualTo(now).test(now));
    assertFalse(localTimeEqualTo(now).test(now.plusNanos(1)));
    assertFalse(localTimeEqualTo(now).test(now.minusNanos(1)));

    assertTrue(localTimeEqualTo(ObjectFromLocalTime::getSource, now).test(new ObjectFromLocalTime(now, LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeEqualTo(ObjectFromLocalTime::getSource, now).test(new ObjectFromLocalTime(now.plusNanos(1), LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeEqualTo(ObjectFromLocalTime::getSource, now).test(new ObjectFromLocalTime(now.minusNanos(1), LocalTime.now(), LocalTime.now())));
  }

  @Test
  public void testLocalTimeEqualToNullValues() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeEqualTo(now).test(null));

    assertFalse(localTimeEqualTo(null).test(null));

    assertFalse(localTimeEqualTo(null, now).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeEqualTo(ObjectFromLocalTime::getSource, now).test(null));
    assertFalse(localTimeEqualTo(ObjectFromLocalTime::getSource, now).test(new ObjectFromLocalTime(null, LocalTime.now(), LocalTime.now())));

    assertFalse(localTimeEqualTo(null, null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeEqualTo(ObjectFromLocalTime::getSource, null).test(null));
    assertFalse(localTimeEqualTo(ObjectFromLocalTime::getSource, null).test(new ObjectFromLocalTime(null, LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localTimeAfterNow

  @Test
  public void testLocalTimeAfterNow() {
    assertTrue(localTimeAfterNow().test(LocalTime.now().plusSeconds(1)));
    assertFalse(localTimeAfterNow().test(LocalTime.now().minusSeconds(1)));

    assertFalse(localTimeAfterNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
    assertTrue(localTimeAfterNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(LocalTime.now().plusSeconds(1), LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeAfterNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(LocalTime.now().minusSeconds(1), LocalTime.now(), LocalTime.now())));
  }

  @Test
  public void testLocalTimeAfterNowNullValues() {
    assertFalse(localTimeAfterNow().test(null));

    assertFalse(localTimeAfterNow(null).test(null));
    assertFalse(localTimeAfterNow(null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeAfterNow(ObjectFromLocalTime::getSource).test(null));
    assertFalse(localTimeAfterNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(null, LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localTimeBeforeNow

  @Test
  public void testLocalTimeBeforeNow() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeBeforeNow().test(now.plusSeconds(1)));
    assertTrue(localTimeBeforeNow().test(now.minusSeconds(1)));

    assertFalse(localTimeBeforeNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(now.plusSeconds(1), now, now)));
    assertTrue(localTimeBeforeNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(now.minusSeconds(1), now, now)));
  }

  @Test
  public void testLocalTimeBeforeNowNullValues() {
    assertFalse(localTimeBeforeNow().test(null));

    assertFalse(localTimeBeforeNow(null).test(null));
    assertFalse(localTimeBeforeNow(null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
    assertFalse(localTimeBeforeNow(ObjectFromLocalTime::getSource).test(null));
    assertFalse(localTimeBeforeNow(ObjectFromLocalTime::getSource).test(new ObjectFromLocalTime(null, LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localDateAfter

  @Test
  public void testLocalTimeAfter() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeAfter(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeAfter(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.plusNanos(1), now)));
    assertTrue(localTimeAfter(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalTimeAfterNullValues() {
    assertFalse(localTimeAfter(null).test(null));
    assertFalse(localTimeAfter(null).test(LocalTime.now()));
    assertFalse(localTimeAfter(LocalTime.now()).test(null));

    assertFalse(localTimeAfter(null, (LocalTime) null).test(null));
    assertFalse(localTimeAfter(ObjectFromLocalTime::getSource, (LocalTime) null).test(null));
    assertFalse(localTimeAfter(null, LocalTime.now()).test(null));
    assertFalse(localTimeAfter(null, (LocalTime) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));

    assertFalse(localTimeAfter(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeAfter(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeAfter(null, ObjectFromLocalTime::getSource).test(null));
    assertFalse(localTimeAfter(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localTimeAfterOrEqual

  @Test
  public void testLocalTimeAfterOrEqual() {
    final LocalTime now = LocalTime.now();

    assertTrue(localTimeAfterOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeAfterOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.plusNanos(1), now)));
    assertTrue(localTimeAfterOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalTimeAfterOrEqualNullValues() {
    assertFalse(localTimeAfterOrEqual(null).test(null));
    assertFalse(localTimeAfterOrEqual(null).test(LocalTime.now()));
    assertFalse(localTimeAfterOrEqual(LocalTime.now()).test(null));

    assertFalse(localTimeAfterOrEqual(null, (LocalTime) null).test(null));
    assertFalse(localTimeAfterOrEqual(ObjectFromLocalTime::getSource, (LocalTime) null).test(null));
    assertFalse(localTimeAfterOrEqual(null, LocalTime.now()).test(null));
    assertFalse(localTimeAfterOrEqual(null, (LocalTime) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));

    assertFalse(localTimeAfterOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeAfterOrEqual(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeAfterOrEqual(null, ObjectFromLocalTime::getSource).test(null));
    assertFalse(localTimeAfterOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localDateBefore

  @Test
  public void testLocalTimeBefore() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeBefore(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBefore(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.plusNanos(1), now)));
    assertFalse(localTimeBefore(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalTimeBeforeNullValues() {
    assertFalse(localTimeBefore(null).test(null));
    assertFalse(localTimeBefore(null).test(LocalTime.now()));
    assertFalse(localTimeBefore(LocalTime.now()).test(null));

    assertFalse(localTimeBefore(null, (LocalTime) null).test(null));
    assertFalse(localTimeBefore(ObjectFromLocalTime::getSource, (LocalTime) null).test(null));
    assertFalse(localTimeBefore(null, LocalTime.now()).test(null));
    assertFalse(localTimeBefore(null, (LocalTime) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));

    assertFalse(localTimeBefore(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBefore(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBefore(null, ObjectFromLocalTime::getSource).test(null));
    assertFalse(localTimeBefore(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localTimeBeforeOrEqual

  @Test
  public void testLocalTimeBeforeOrEqual() {
    final LocalTime now = LocalTime.now();

    assertTrue(localTimeBeforeOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBeforeOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.plusNanos(1), now)));
    assertFalse(localTimeBeforeOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
  }

  @Test
  public void testLocalTimeBeforeOrEqualNullValues() {
    assertFalse(localTimeBeforeOrEqual(null).test(null));
    assertFalse(localTimeBeforeOrEqual(null).test(LocalTime.now()));
    assertFalse(localTimeBeforeOrEqual(LocalTime.now()).test(null));

    assertFalse(localTimeBeforeOrEqual(null, (LocalTime) null).test(null));
    assertFalse(localTimeBeforeOrEqual(ObjectFromLocalTime::getSource, (LocalTime) null).test(null));
    assertFalse(localTimeBeforeOrEqual(null, LocalTime.now()).test(null));
    assertFalse(localTimeBeforeOrEqual(null, (LocalTime) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));

    assertFalse(localTimeBeforeOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBeforeOrEqual(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBeforeOrEqual(null, ObjectFromLocalTime::getSource).test(null));
    assertFalse(localTimeBeforeOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(LocalTime.now(), LocalTime.now(), LocalTime.now())));
  }

  // endregion

  // region localDateBetween

  @Test
  public void testLocalTimeBetween() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeBetween(now, now).test(now));
    assertFalse(localTimeBetween(now.minusNanos(1), now).test(now));
    assertFalse(localTimeBetween(now, now.plusNanos(1)).test(now));
    assertTrue(localTimeBetween(now.minusNanos(1), now.plusNanos(1)).test(now));
    assertFalse(localTimeBetween(now.minusNanos(1), now.plusNanos(1)).test(now.plusNanos(2)));
    assertFalse(localTimeBetween(now.minusNanos(1), now.plusNanos(1)).test(now.minusNanos(2)));

    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now, now).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), now).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now, now.plusNanos(1)).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalTime(now.plusNanos(2), now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalTime(now.minusNanos(2), now, now)));

    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now.plusNanos(2), now.minusNanos(1), now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now.minusNanos(2), now.minusNanos(1), now)));

    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now.plusNanos(1))));
    assertTrue(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now.plusNanos(1))));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.plusNanos(2), now, now.plusNanos(1))));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.minusNanos(2), now, now.plusNanos(1))));

    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now.plusNanos(1))));
    assertTrue(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.plusNanos(2), now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.minusNanos(2), now.minusNanos(1), now.plusNanos(1))));

  }

  @Test
  public void testLocalTimeBetweenNullValues() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeBetween(null, null).test(null));
    assertFalse(localTimeBetween(now, null).test(now));
    assertFalse(localTimeBetween(null, now).test(now));
    assertFalse(localTimeBetween(now, now).test(null));

    assertFalse(localTimeBetween((Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (LocalTime) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetween((Function<ObjectFromLocalTime, LocalTime>) null, now, (LocalTime) null).test(null));
    assertFalse(localTimeBetween((Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null, now).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now, (LocalTime) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (LocalTime) null, now).test(null));
    assertFalse(localTimeBetween((Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null, (LocalTime) null).test(new ObjectFromLocalTime(now, now, now)));

    assertFalse(localTimeBetween(null, (Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetween(null, ObjectFromLocalTime::getMin, (LocalTime) null).test(null));
    assertFalse(localTimeBetween(null, (Function<ObjectFromLocalTime, LocalTime>) null, now).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, (LocalTime) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, now).test(null));
    assertFalse(localTimeBetween(null, (Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null).test(new ObjectFromLocalTime(now, now, now)));

    assertFalse(localTimeBetween(null, (LocalTime) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (LocalTime) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(null, now, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(null, (LocalTime) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, now, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (LocalTime) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetween(null, (LocalTime) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(now, now, now)));

    assertFalse(localTimeBetween(null, (Function<ObjectFromLocalTime, LocalTime>) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(null, ObjectFromLocalTime::getMin, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(null, (Function<ObjectFromLocalTime, LocalTime>) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetween(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetween(null, (Function<ObjectFromLocalTime, LocalTime>) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(now, now, now)));

  }

  // endregion

  // region localDateBetweenOrEqual

  @Test
  public void testLocalTimeBetweenOrEqual() {
    final LocalTime now = LocalTime.now();

    assertTrue(localTimeBetweenOrEqual(now, now).test(now));
    assertTrue(localTimeBetweenOrEqual(now.minusNanos(1), now).test(now));
    assertTrue(localTimeBetweenOrEqual(now, now.plusNanos(1)).test(now));
    assertTrue(localTimeBetweenOrEqual(now.minusNanos(1), now.plusNanos(1)).test(now));
    assertFalse(localTimeBetweenOrEqual(now.minusNanos(1), now.plusNanos(1)).test(now.plusNanos(2)));
    assertFalse(localTimeBetweenOrEqual(now.minusNanos(1), now.plusNanos(1)).test(now.minusNanos(2)));

    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now, now).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), now).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now, now.plusNanos(1)).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalTime(now, now, now)));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalTime(now.plusNanos(2), now, now)));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), now.plusNanos(1)).test(new ObjectFromLocalTime(now.minusNanos(2), now, now)));

    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now.plusNanos(2), now.minusNanos(1), now)));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, now.plusNanos(1)).test(new ObjectFromLocalTime(now.minusNanos(2), now.minusNanos(1), now)));

    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now.plusNanos(1))));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now.plusNanos(1))));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.plusNanos(2), now, now.plusNanos(1))));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now.minusNanos(1), ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.minusNanos(2), now, now.plusNanos(1))));

    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now.minusNanos(1), now)));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now, now.plusNanos(1))));
    assertTrue(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.plusNanos(2), now.minusNanos(1), now.plusNanos(1))));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now.minusNanos(2), now.minusNanos(1), now.plusNanos(1))));

  }

  @Test
  public void testLocalTimeBetweenOrEqualNullValues() {
    final LocalTime now = LocalTime.now();

    assertFalse(localTimeBetweenOrEqual(null, null).test(null));
    assertFalse(localTimeBetweenOrEqual(now, null).test(now));
    assertFalse(localTimeBetweenOrEqual(null, now).test(now));
    assertFalse(localTimeBetweenOrEqual(now, now).test(null));

    assertFalse(localTimeBetweenOrEqual((Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (LocalTime) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual((Function<ObjectFromLocalTime, LocalTime>) null, now, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual((Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null, now).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (LocalTime) null, now).test(null));
    assertFalse(localTimeBetweenOrEqual((Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null, (LocalTime) null).test(new ObjectFromLocalTime(now, now, now)));

    assertFalse(localTimeBetweenOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual(null, ObjectFromLocalTime::getMin, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null, now).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, (LocalTime) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, now).test(null));
    assertFalse(localTimeBetweenOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null, (LocalTime) null).test(new ObjectFromLocalTime(now, now, now)));

    assertFalse(localTimeBetweenOrEqual(null, (LocalTime) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (LocalTime) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(null, now, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(null, (LocalTime) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, now, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (LocalTime) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetweenOrEqual(null, (LocalTime) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(now, now, now)));

    assertFalse(localTimeBetweenOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(null, ObjectFromLocalTime::getMin, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, (Function<ObjectFromLocalTime, LocalTime>) null).test(null));
    assertFalse(localTimeBetweenOrEqual(ObjectFromLocalTime::getSource, (Function<ObjectFromLocalTime, LocalTime>) null, ObjectFromLocalTime::getMax).test(null));
    assertFalse(localTimeBetweenOrEqual(null, (Function<ObjectFromLocalTime, LocalTime>) null, (Function<ObjectFromLocalTime, LocalTime>) null).test(new ObjectFromLocalTime(now, now, now)));

  }

  // endregion

  // region multi thread test

  @Test
  public void testLocalDatePredicateMultiThreadMustBeTrue() throws InterruptedException {
    final int CONCURRENT_RUNNABLE = 100_000;
    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();
    final ExecutorService executorService = Executors.newFixedThreadPool(10);
    final LocalTime now = LocalTime.now();

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          assertThatCode(() -> {
            resultsOne.add(localTimeBetween(ObjectFromLocalTime::getSource, ObjectFromLocalTime::getMin, ObjectFromLocalTime::getMax).test(new ObjectFromLocalTime(now, now.minusNanos(1), now.plusNanos(1))));
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

  private static class ObjectFromLocalTime extends ObjectFrom<LocalTime> {

    private final LocalTime min;

    private final LocalTime max;

    public ObjectFromLocalTime(final LocalTime source, final LocalTime min, final LocalTime max) {
      super(source, null);
      this.min = min;
      this.max = max;
    }

    public LocalTime getMin() {
      return min;
    }

    public LocalTime getMax() {
      return max;
    }

  }

}
