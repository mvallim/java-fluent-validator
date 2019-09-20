package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeBetween;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeEqualTo;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeGreaterThan;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeGreaterThanOrEqual;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeLessThan;
import static br.com.fluentvalidator.predicate.DateTimePredicate.dateTimeLessThanOrEqual;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

public class DateTimePredicateTest {

  @Test
  public void testNullDateTimeEqualTo() {
    assertFalse(dateTimeEqualTo(null, null).test(null));
    assertFalse(dateTimeEqualTo(null, null).test("2019-09-19"));
    assertFalse(dateTimeEqualTo(null, "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeEqualTo(null, "YYYY-MM-DD").test(null));
    assertFalse(dateTimeEqualTo("2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeEqualTo("2019-09-19", null).test(null));
    assertFalse(dateTimeEqualTo("2019-09-19", null).test("2019-09-19"));
  }

  @Test
  public void testDateTimeEqualToInvalid() {
    assertThatThrownBy(() -> dateTimeEqualTo("2019-09-19", "yyyy-MM-dd").test("2019/09-19"))
        .isInstanceOf(IllegalArgumentException.class);
    assertThatThrownBy(() -> dateTimeEqualTo("2019-09-19", "yyyy/MM-dd").test("2019-09-19"))
        .isInstanceOf(IllegalArgumentException.class);
    assertThatThrownBy(() -> dateTimeEqualTo("2019/09-19", "yyyy-MM-dd").test("2019-09-19"))
        .isInstanceOf(IllegalArgumentException.class);
  }

  @Test
  public void testDateTimeEqualToYear() {
    assertTrue(dateTimeEqualTo("2019", "yyyy").test("2019"));
    assertFalse(dateTimeEqualTo("2019", "yyyy").test("2018"));
  }

  @Test
  public void testDateTimeEqualToMonth() {
    assertTrue(dateTimeEqualTo("02", "MM").test("02"));
    assertFalse(dateTimeEqualTo("02", "MM").test("03"));
  }

  @Test
  public void testDateTimeEqualToDay() {
    assertTrue(dateTimeEqualTo("02", "dd").test("02"));
    assertFalse(dateTimeEqualTo("02", "dd").test("03"));
  }

  @Test
  public void testDateTimeEqualToDate() {
    assertTrue(dateTimeEqualTo("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
    assertFalse(dateTimeEqualTo("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
  }

  @Test
  public void testDateTimeEqualToHour() {
    assertTrue(dateTimeEqualTo("03", "HH").test("03"));
    assertFalse(dateTimeEqualTo("03", "HH").test("04"));
  }

  @Test
  public void testDateTimeEqualToMinute() {
    assertTrue(dateTimeEqualTo("03", "mm").test("03"));
    assertFalse(dateTimeEqualTo("03", "mm").test("04"));
  }

  @Test
  public void testDateTimeEqualToSecond() {
    assertTrue(dateTimeEqualTo("03", "ss").test("03"));
    assertFalse(dateTimeEqualTo("03", "ss").test("04"));
  }

  @Test
  public void testDateTimeEqualToTime() {
    assertTrue(dateTimeEqualTo("03:03:03", "HH:mm:ss").test("03:03:03"));
    assertFalse(dateTimeEqualTo("03:03:03", "HH:mm:ss").test("03:03:04"));
  }

  @Test
  public void testDateTimeEqualToDateTime() {
    assertTrue(
        dateTimeEqualTo("2019-09-19 03:03:03", "yyyy-MM-dd HH:mm:ss").test("2019-09-19 03:03:03"));
    assertFalse(
        dateTimeEqualTo("2019-09-19 03:03:03", "yyyy-MM-dd HH:mm:ss").test("2019-09-19 03:03:04"));
  }

  @Test
  public void testNullDateTimeGreaterThan() {
    assertFalse(dateTimeGreaterThan(null, null).test(null));
    assertFalse(dateTimeGreaterThan(null, null).test("2019-09-19"));
    assertFalse(dateTimeGreaterThan(null, "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeGreaterThan(null, "YYYY-MM-DD").test(null));
    assertFalse(dateTimeGreaterThan("2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeGreaterThan("2019-09-19", null).test(null));
    assertFalse(dateTimeGreaterThan("2019-09-19", null).test("2019-09-19"));
  }

  @Test
  public void testDateTimeGreaterThanInvalid() {
    assertThatThrownBy(() -> dateTimeGreaterThan("2019-09-19", "yyyy-MM-dd").test("2019/09-19"))
        .isInstanceOf(IllegalArgumentException.class);
    assertThatThrownBy(() -> dateTimeGreaterThan("2019-09-19", "yyyy/MM-dd").test("2019-09-19"))
        .isInstanceOf(IllegalArgumentException.class);
    assertThatThrownBy(() -> dateTimeGreaterThan("2019/09-19", "yyyy-MM-dd").test("2019-09-19"))
        .isInstanceOf(IllegalArgumentException.class);
  }

  @Test
  public void testDateTimeGreaterThanYear() {
    assertTrue(dateTimeGreaterThan("2018", "yyyy").test("2019"));
    assertFalse(dateTimeGreaterThan("2019", "yyyy").test("2019"));
    assertFalse(dateTimeGreaterThan("2020", "yyyy").test("2019"));
  }

  @Test
  public void testDateTimeGreaterThanMonth() {
    assertTrue(dateTimeGreaterThan("08", "MM").test("09"));
    assertFalse(dateTimeGreaterThan("09", "MM").test("09"));
    assertFalse(dateTimeGreaterThan("10", "MM").test("09"));
  }

  @Test
  public void testDateTimeGreaterThanDay() {
    assertTrue(dateTimeGreaterThan("18", "dd").test("19"));
    assertFalse(dateTimeGreaterThan("19", "dd").test("19"));
    assertFalse(dateTimeGreaterThan("20", "dd").test("19"));
  }

  @Test
  public void testDateTimeGreaterThanDate() {
    assertTrue(dateTimeGreaterThan("2019-09-18", "yyyy-MM-dd").test("2019-09-19"));
    assertFalse(dateTimeGreaterThan("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
    assertFalse(dateTimeGreaterThan("2019-09-20", "yyyy-MM-dd").test("2019-09-19"));
  }

  @Test
  public void testDateTimeGreaterThanDateTime() {
    assertTrue(dateTimeGreaterThan("2019-09-19 03:03:03", "yyyy-MM-dd HH:mm:ss")
        .test("2019-09-19 03:03:04"));
    assertFalse(dateTimeGreaterThan("2019-09-19 03:03:04", "yyyy-MM-dd HH:mm:ss")
        .test("2019-09-19 03:03:04"));
    assertFalse(dateTimeGreaterThan("2019-09-19 03:03:05", "yyyy-MM-dd HH:mm:ss")
        .test("2019-09-19 03:03:04"));
  }

  @Test
  public void testDateTimeGreaterThanHour() {
    assertTrue(dateTimeGreaterThan("03", "HH").test("04"));
    assertFalse(dateTimeGreaterThan("04", "HH").test("04"));
    assertFalse(dateTimeGreaterThan("05", "HH").test("04"));
  }

  @Test
  public void testDateTimeGreaterThanMinute() {
    assertTrue(dateTimeGreaterThan("03", "mm").test("04"));
    assertFalse(dateTimeGreaterThan("04", "mm").test("04"));
    assertFalse(dateTimeGreaterThan("05", "mm").test("04"));
  }

  @Test
  public void testDateTimeGreaterThanSecond() {
    assertTrue(dateTimeGreaterThan("03", "ss").test("04"));
    assertFalse(dateTimeGreaterThan("04", "ss").test("04"));
    assertFalse(dateTimeGreaterThan("05", "ss").test("04"));
  }

  @Test
  public void testDateTimeGreaterThanTime() {
    assertTrue(dateTimeGreaterThan("03:03:03", "HH:mm:ss").test("03:03:04"));
    assertFalse(dateTimeGreaterThan("03:03:04", "HH:mm:ss").test("03:03:04"));
    assertFalse(dateTimeGreaterThan("03:03:05", "HH:mm:ss").test("03:03:04"));
  }

  @Test
  public void testNullDateTimeLessThanThan() {
    assertFalse(dateTimeLessThan(null, null).test(null));
    assertFalse(dateTimeLessThan(null, null).test("2019-09-19"));
    assertFalse(dateTimeLessThan(null, "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeLessThan(null, "YYYY-MM-DD").test(null));
    assertFalse(dateTimeLessThan("2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeLessThan("2019-09-19", null).test(null));
    assertFalse(dateTimeLessThan("2019-09-19", null).test("2019-09-19"));
  }

  @Test
  public void testDateTimeLessThanInvalid() {
    assertThatThrownBy(() -> dateTimeLessThan("2019-09-19", "yyyy-MM-dd").test("2019/09-19"))
        .isInstanceOf(IllegalArgumentException.class);
    assertThatThrownBy(() -> dateTimeLessThan("2019-09-19", "yyyy/MM-dd").test("2019-09-19"))
        .isInstanceOf(IllegalArgumentException.class);
    assertThatThrownBy(() -> dateTimeLessThan("2019/09-19", "yyyy-MM-dd").test("2019-09-19"))
        .isInstanceOf(IllegalArgumentException.class);
  }

  @Test
  public void testDateTimeLessThanYear() {
    assertTrue(dateTimeLessThan("2019", "yyyy").test("2018"));
    assertFalse(dateTimeLessThan("2018", "yyyy").test("2018"));
    assertFalse(dateTimeLessThan("2017", "yyyy").test("2018"));
  }

  @Test
  public void testDateTimeLessThanMonth() {
    assertTrue(dateTimeLessThan("09", "MM").test("08"));
    assertFalse(dateTimeLessThan("08", "MM").test("08"));
    assertFalse(dateTimeLessThan("07", "MM").test("08"));
  }

  @Test
  public void testDateTimeLessThanDay() {
    assertTrue(dateTimeLessThan("09", "dd").test("08"));
    assertFalse(dateTimeLessThan("08", "dd").test("08"));
    assertFalse(dateTimeLessThan("07", "dd").test("08"));
  }

  @Test
  public void testDateTimeLessThanDate() {
    assertTrue(dateTimeLessThan("2019-09-19", "yyyy-MM-dd").test("2019-09-18"));
    assertFalse(dateTimeLessThan("2019-09-18", "yyyy-MM-dd").test("2019-09-18"));
    assertFalse(dateTimeLessThan("2019-09-17", "yyyy-MM-dd").test("2019-09-18"));
  }

  @Test
  public void testDateTimeLessThanDateTime() {
    assertTrue(
        dateTimeLessThan("2019-09-19 03:03:04", "yyyy-MM-dd HH:mm:ss").test("2019-09-18 03:03:04"));
    assertFalse(
        dateTimeLessThan("2019-09-18 03:03:04", "yyyy-MM-dd HH:mm:ss").test("2019-09-18 03:03:04"));
    assertFalse(
        dateTimeLessThan("2019-09-17 03:03:04", "yyyy-MM-dd HH:mm:ss").test("2019-09-18 03:03:04"));
  }

  @Test
  public void testDateTimeLessThanTime() {
    assertTrue(dateTimeLessThan("03:03:04", "HH:mm:ss").test("03:03:03"));
    assertFalse(dateTimeLessThan("03:03:03", "HH:mm:ss").test("03:03:03"));
    assertFalse(dateTimeLessThan("03:03:02", "HH:mm:ss").test("03:03:03"));
  }

  @Test
  public void testDateTimeLessThanHour() {
    assertTrue(dateTimeLessThan("19", "HH").test("18"));
    assertFalse(dateTimeLessThan("18", "HH").test("18"));
    assertFalse(dateTimeLessThan("17", "HH").test("18"));
  }

  @Test
  public void testDateTimeLessThanMinute() {
    assertTrue(dateTimeLessThan("09", "mm").test("08"));
    assertFalse(dateTimeLessThan("08", "mm").test("08"));
    assertFalse(dateTimeLessThan("07", "mm").test("08"));
  }

  @Test
  public void testDateTimeLessThanSecond() {
    assertTrue(dateTimeLessThan("09", "ss").test("08"));
    assertFalse(dateTimeLessThan("08", "ss").test("08"));
    assertFalse(dateTimeLessThan("07", "ss").test("08"));
  }

  @Test
  public void testNullDateTimeGreaterThanOrEqual() {
    assertFalse(dateTimeGreaterThanOrEqual(null, null).test(null));
    assertFalse(dateTimeGreaterThanOrEqual(null, null).test("2019-09-19"));
    assertFalse(dateTimeGreaterThanOrEqual(null, "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeGreaterThanOrEqual(null, "YYYY-MM-DD").test(null));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19", null).test(null));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19", null).test("2019-09-19"));
  }

  @Test
  public void testDateTimeGreaterThanOrEqual() {
    assertTrue(dateTimeGreaterThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
    assertTrue(dateTimeGreaterThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
    assertFalse(dateTimeGreaterThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-18"));
  }

  @Test
  public void testNullDateTimeLessThanOrEqual() {
    assertFalse(dateTimeLessThanOrEqual(null, null).test(null));
    assertFalse(dateTimeLessThanOrEqual(null, null).test("2019-09-19"));
    assertFalse(dateTimeLessThanOrEqual(null, "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeLessThanOrEqual(null, "YYYY-MM-DD").test(null));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19", null).test(null));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19", null).test("2019-09-19"));
  }

  @Test
  public void testDateLessTimeThanOrEqual() {
    assertTrue(dateTimeLessThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-18"));
    assertTrue(dateTimeLessThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
    assertFalse(dateTimeLessThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
  }

  @Test
  public void testNullDateTimeBetween() {
    assertFalse(dateTimeBetween("2019-09-19", null, null).test(null));
    assertFalse(dateTimeBetween(null, "2019-09-19", null).test(null));
    assertFalse(dateTimeBetween(null, null, "YYYY-MM-DD").test(null));
    assertFalse(dateTimeBetween(null, null, null).test("2019-09-19"));
    assertFalse(dateTimeBetween("2019-09-19", "2019-09-19", null).test(null));
    assertFalse(dateTimeBetween(null, "2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeBetween(null, null, "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeBetween("2019-09-19", "2019-09-19", "YYYY-MM-DD").test(null));
    assertFalse(dateTimeBetween(null, "2019-09-19", "YYYY-MM-DD").test("2019-09-19"));
    assertFalse(dateTimeBetween(null, null, "YYYY-MM-DD").test("2019-09-19"));
  }

  @Test
  public void testDateTimeBetween() {
    assertTrue(dateTimeBetween("2019-09-19", "2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
    assertTrue(dateTimeBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-20"));
    assertTrue(dateTimeBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-18"));
    assertTrue(dateTimeBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-19"));
    assertFalse(dateTimeBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-17"));
  }

  @Test
  public void testDateTimePredicateMultiThreadMustBeTrue() throws InterruptedException {

    final int CONCURRENT_RUNNABLE = 100000;

    final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();

    final ExecutorService executorService = Executors.newFixedThreadPool(10);

    for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
      executorService.submit(new Runnable() {
        @Override
        public void run() {
          try {
            resultsOne.add(dateTimeBetween("2018-06-22T10:00:00", "2018-06-22T10:00:00",
                "yyyy-MM-dd'T'HH:mm:ss").test("2018-06-22T10:00:00"));
          } catch (final Exception e) {
            e.printStackTrace();
          }
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
