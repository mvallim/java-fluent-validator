package br.com.fluentvalidator.predicate;

import org.junit.Test;

import java.time.LocalDate;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static br.com.fluentvalidator.predicate.LocalDatePredicate.*;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.*;

public class LocalDatePredicateTest {

  //// localDateAfterToday()

  @Test
  public void testLocalDateAfterToday() {
    assertFalse(localDateAfterToday().test(LocalDate.now()));

    assertFalse(localDateAfterToday().test(LocalDate.now().minusDays(1)));
    assertFalse(localDateAfterToday().test(LocalDate.now().minusMonths(1)));
    assertFalse(localDateAfterToday().test(LocalDate.now().minusYears(1)));

    assertTrue(localDateAfterToday().test(LocalDate.now().plusDays(1)));
    assertTrue(localDateAfterToday().test(LocalDate.now().plusMonths(1)));
    assertTrue(localDateAfterToday().test(LocalDate.now().plusYears(1)));

    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().minusYears(1), LocalDate.now())));

    assertTrue(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertTrue(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertTrue(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().plusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateAfterToday() {
    assertFalse(localDateAfterToday().test((LocalDate) null));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateAfterToday(ObjectFrom<LocalDate>::getSource).test(null));
  }

  //// localDateBeforeToday()

  @Test
  public void testLocalDateBeforeToday() {
    assertFalse(localDateBeforeToday().test(LocalDate.now()));

    assertTrue(localDateBeforeToday().test(LocalDate.now().minusDays(1)));
    assertTrue(localDateBeforeToday().test(LocalDate.now().minusMonths(1)));
    assertTrue(localDateBeforeToday().test(LocalDate.now().minusYears(1)));

    assertFalse(localDateBeforeToday().test(LocalDate.now().plusDays(1)));
    assertFalse(localDateBeforeToday().test(LocalDate.now().plusMonths(1)));
    assertFalse(localDateBeforeToday().test(LocalDate.now().plusYears(1)));

    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));

    assertTrue(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertTrue(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertTrue(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().minusYears(1), LocalDate.now())));

    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(LocalDate.now().plusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBeforeToday() {
    assertFalse(localDateBeforeToday().test((LocalDate) null));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateBeforeToday(ObjectFrom<LocalDate>::getSource).test(null));
  }

  //// localDateAfter(Function)

  @Test
  public void testLocalDateAfter() {
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));

    assertTrue(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertTrue(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertTrue(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateAfter() {
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(null));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, LocalDate.now())));
    assertFalse(localDateAfter(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now(), null)));
  }

  //// localDateBefore(Function)

  @Test
  public void testLocalDateBefore() {
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().plusDays(1), LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().plusMonths(1), LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().plusYears(1), LocalDate.now())));

    assertTrue(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().minusDays(1), LocalDate.now())));
    assertTrue(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().minusMonths(1), LocalDate.now())));
    assertTrue(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now().minusYears(1), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBefore() {
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(null));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, null)));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(null, LocalDate.now())));
    assertFalse(localDateBefore(ObjectFrom<LocalDate>::getSource, ObjectFrom<LocalDate>::getTarget).test(new ObjectFrom<LocalDate>(LocalDate.now(), null)));
  }

  //// localDateBetween(Function, LocalDate,LocalDate)

  @Test
  public void testLocalDateBetweenLocalDate() {
    assertTrue(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now(), LocalDate.now().plusDays(1)).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));
    assertFalse(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now()).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));

    assertFalse(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(new ObjectFrom<LocalDate>(LocalDate.now().minusDays(2), LocalDate.now())));
    assertFalse(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(new ObjectFrom<LocalDate>(LocalDate.now().plusDays(2), LocalDate.now())));
  }

  @Test
  public void testNullObjectLocalDateBetweenLocalDate() {
    assertFalse(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(new ObjectFrom<LocalDate>(null, LocalDate.now())));
  }

  //// multi thread test

  @Test
  public void testLocalDatePredicateMultiThreadMustBeTrue() throws InterruptedException {
    final int CONCURRENT_RUNNABLE = 100000;
    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();
    final ExecutorService executorService = Executors.newFixedThreadPool(10);

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          assertThatCode(() -> {
            resultsOne.add(localDateBetween(ObjectFrom<LocalDate>::getSource, LocalDate.now().minusDays(1), LocalDate.now().plusDays(1)).test(new ObjectFrom<LocalDate>(LocalDate.now(), LocalDate.now())));
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

}
