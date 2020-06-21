package br.com.fluentvalidator.predicate;

import org.junit.Test;

import java.time.LocalDateTime;

import static br.com.fluentvalidator.predicate.LocalDateTimePredicate.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class LocalDateTimePredicateTest {

  //// localDateTimeAfterNow()

  @Test
  public void testLocalDateTimeAfterNow() {
    assertFalse(localDateTimeAfterNow().test(LocalDateTime.now()));

    assertTrue(localDateTimeAfterNow().test(LocalDateTime.now().plusSeconds(10)));
    assertTrue(localDateTimeAfterNow().test(LocalDateTime.now().plusNanos(100)));

    assertFalse(localDateTimeAfterNow().test(LocalDateTime.now().minusSeconds(10)));
    assertFalse(localDateTimeAfterNow().test(LocalDateTime.now().minusNanos(100)));

    assertFalse(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(new ObjectFrom<LocalDateTime>(LocalDateTime.now(), LocalDateTime.now())));

    assertTrue(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(new ObjectFrom<LocalDateTime>(LocalDateTime.now().plusSeconds(10), LocalDateTime.now())));
    assertTrue(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(new ObjectFrom<LocalDateTime>(LocalDateTime.now().plusNanos(100), LocalDateTime.now())));

    assertFalse(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(new ObjectFrom<LocalDateTime>(LocalDateTime.now().minusSeconds(10), LocalDateTime.now())));
    assertFalse(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(new ObjectFrom<LocalDateTime>(LocalDateTime.now().minusNanos(100), LocalDateTime.now())));
  }

  @Test
  public void testNullObjectLocalDateTimeAfterNow() {
    assertFalse(localDateTimeAfterNow().test(null));
    assertFalse(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(new ObjectFrom<LocalDateTime>(null, LocalDateTime.now())));
    assertFalse(localDateTimeAfterNow(ObjectFrom<LocalDateTime>::getSource).test(null));
  }



}
