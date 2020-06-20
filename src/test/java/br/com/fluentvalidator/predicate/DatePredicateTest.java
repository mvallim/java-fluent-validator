package br.com.fluentvalidator.predicate;

import org.junit.Test;

import java.time.LocalDate;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static br.com.fluentvalidator.predicate.DatePredicate.*;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.*;

public class DatePredicateTest {

	private static final String YYYY_MM_DD = "yyyy-MM-dd";

	//// dateTimeEqualTo

	@Test
	public void testNullDateTimeEqualTo() {
		assertFalse(dateEqualTo(null, null).test(null));
		assertFalse(dateEqualTo(null, null).test("2019-09-19"));
		assertFalse(dateEqualTo(null, YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateEqualTo(null, YYYY_MM_DD).test(null));
		assertFalse(dateEqualTo("2019-09-19", YYYY_MM_DD).test(null));
		assertFalse(dateEqualTo("2019-09-19", null).test(null));
		assertFalse(dateEqualTo("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateEqualToInvalid() {
		assertFalse(dateEqualTo("2019-09-19", YYYY_MM_DD).test("2019-09-40"));
		assertFalse(dateEqualTo("2019-09-19", YYYY_MM_DD).test("2019/09-19"));
		assertFalse(dateEqualTo("2019-09-19", "yyyy/MM-dd").test("2019-09-19"));
		assertFalse(dateEqualTo("2019/09-19", YYYY_MM_DD).test("2019-09-19"));
	}

	@Test
	public void testDateEqualTo() {
		assertTrue(dateEqualTo("2019-09-19", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateEqualTo("2019-09-20", YYYY_MM_DD).test("2019-09-19"));
	}

	//// dateGreaterThan

	@Test
	public void testNullDateTimeGreaterThan() {
		assertFalse(dateGreaterThan(null, null).test(null));
		assertFalse(dateGreaterThan(null, null).test("2019-09-19"));
		assertFalse(dateGreaterThan(null, YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateGreaterThan(null, YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThan("2019-09-19", YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThan("2019-09-19", null).test(null));
		assertFalse(dateGreaterThan("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateGreaterThanInvalid() {
		assertFalse(dateGreaterThan("2019-09-19", YYYY_MM_DD).test("2019-09-40"));
		assertFalse(dateGreaterThan("2019-09-19", YYYY_MM_DD).test("2019/09-19"));
		assertFalse(dateGreaterThan("2019-09-19", "yyyy/MM-dd").test("2019-09-19"));
		assertFalse(dateGreaterThan("2019/09-19", YYYY_MM_DD).test("2019-09-19"));
	}

	@Test
	public void testDateGreaterThan() {
		assertTrue(dateGreaterThan("2019-09-18", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateGreaterThan("2019-09-19", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateGreaterThan("2019-09-20", YYYY_MM_DD).test("2019-09-19"));
	}

	//// dateLessThan

	@Test
	public void testNullDateTimeLessThan() {
		assertFalse(dateLessThan(null, null).test(null));
		assertFalse(dateLessThan(null, null).test("2019-09-19"));
		assertFalse(dateLessThan(null, "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateLessThan(null, "YYYY-MM-DD").test(null));
		assertFalse(dateLessThan("2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateLessThan("2019-09-19", null).test(null));
		assertFalse(dateLessThan("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateLessThanInvalid() {
		assertFalse(dateLessThan("2019-09-19", YYYY_MM_DD).test("2019/09-19"));
		assertFalse(dateLessThan("2019-09-19", "yyyy/MM-dd").test("2019-09-19"));
		assertFalse(dateLessThan("2019/09-19", YYYY_MM_DD).test("2019-09-19"));
	}

	@Test
	public void testDateLessThan() {
		assertTrue(dateLessThan("2019-09-19", YYYY_MM_DD).test("2019-09-18"));
		assertFalse(dateLessThan("2019-09-18", YYYY_MM_DD).test("2019-09-18"));
		assertFalse(dateLessThan("2019-09-17", YYYY_MM_DD).test("2019-09-18"));
	}

	//// dateGreaterThanOrEqual

	@Test
	public void testNullDateTimeGreaterThanOrEqual() {
		assertFalse(dateGreaterThanOrEqual(null, null).test(null));
		assertFalse(dateGreaterThanOrEqual(null, null).test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual(null, YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual(null, YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", null).test(null));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateGreaterThanOrEqualInvalid() {
		assertFalse(dateGreaterThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019/09-19"));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", "yyyy/MM-dd").test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual("2019/09-19", YYYY_MM_DD).test("2019-09-19"));
	}

	@Test
	public void testDateGreaterThanOrEqual() {
		assertTrue(dateGreaterThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019-09-20"));
		assertTrue(dateGreaterThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019-09-18"));
	}

	//// dateLessThanOrEqual

	@Test
	public void testNullDateTimeLessThanOrEqual() {
		assertFalse(dateLessThanOrEqual(null, null).test(null));
		assertFalse(dateLessThanOrEqual(null, null).test("2019-09-19"));
		assertFalse(dateLessThanOrEqual(null, YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateLessThanOrEqual(null, YYYY_MM_DD).test(null));
		assertFalse(dateLessThanOrEqual("2019-09-19", YYYY_MM_DD).test(null));
		assertFalse(dateLessThanOrEqual("2019-09-19", null).test(null));
		assertFalse(dateLessThanOrEqual("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateLessTimeThanOrEqualInvalid() {
		assertFalse(dateLessThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019/09-19"));
		assertFalse(dateLessThanOrEqual("2019-09-19", "yyyy/MM-dd").test("2019-09-19"));
		assertFalse(dateLessThanOrEqual("2019/09-19", YYYY_MM_DD).test("2019-09-19"));
	}

	@Test
	public void testDateLessTimeThanOrEqual() {
		assertTrue(dateLessThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019-09-18"));
		assertTrue(dateLessThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateLessThanOrEqual("2019-09-19", YYYY_MM_DD).test("2019-09-20"));
	}

	//// dateBetween

	@Test
	public void testNullDateTimeBetween() {
		assertFalse(dateBetween("2019-09-19", null, null).test(null));
		assertFalse(dateBetween(null, "2019-09-19", null).test(null));
		assertFalse(dateBetween(null, null, YYYY_MM_DD).test(null));
		assertFalse(dateBetween(null, null, null).test("2019-09-19"));
		assertFalse(dateBetween("2019-09-19", "2019-09-19", null).test(null));
		assertFalse(dateBetween(null, "2019-09-19", YYYY_MM_DD).test(null));
		assertFalse(dateBetween(null, null, YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateBetween("2019-09-19", "2019-09-19", YYYY_MM_DD).test(null));
		assertFalse(dateBetween(null, "2019-09-19", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateBetween(null, null, YYYY_MM_DD).test("2019-09-19"));
	}

	@Test
	public void testTimeBetweenInvalid() {
		assertFalse(dateBetween("2019-09-19", "2019-09-19", YYYY_MM_DD).test("03/59:59"));
		assertFalse(dateBetween("2019-09-19", "2019-09-19", "yyyy-MM/dd").test("03:59:59"));
		assertFalse(dateBetween("2019-09-19", "2019-09/19", YYYY_MM_DD).test("03:59:59"));
		assertFalse(dateBetween("2019-09/19", "2019-09-19", YYYY_MM_DD).test("03:59:59"));
	}

	@Test
	public void testDateBetween() {
		assertTrue(dateBetween("2019-09-19", "2019-09-19", YYYY_MM_DD).test("2019-09-19"));
		assertTrue(dateBetween("2019-09-18", "2019-09-20", YYYY_MM_DD).test("2019-09-20"));
		assertTrue(dateBetween("2019-09-18", "2019-09-20", YYYY_MM_DD).test("2019-09-18"));
		assertTrue(dateBetween("2019-09-18", "2019-09-20", YYYY_MM_DD).test("2019-09-19"));
		assertFalse(dateBetween("2019-09-18", "2019-09-20", YYYY_MM_DD).test("2019-09-17"));
	}

	//// dateGreaterThan(Function, Function, pattern)

	@Test
	public void testObjectDateTimeGreaterThan() {
		assertTrue(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", "2019-09-18")));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", "2019-09-18")));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-17", "2019-09-18")));
	}

	@Test
	public void testNullObjectDateTimeGreaterThan() {
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateGreaterThan(Function, String, pattern)

	@Test
	public void testObjectDateTimeGreaterThan2() {
		assertTrue(dateGreaterThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", null)));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-17", null)));
	}

	@Test
	public void testNullObjectDateTimeGreaterThan2() {
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateGreaterThan(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateGreaterThanOrEqual(Function, Function, pattern)

	@Test
	public void testObjectDateTimeGreaterThanOrEqual() {
		assertTrue(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", "2019-09-18")));
		assertTrue(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", "2019-09-18")));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-17", "2019-09-18")));
	}

	@Test
	public void testNullObjectDateTimeGreaterThanOrEqual() {
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateGreaterThanOrEqual(Function, String, pattern)

	@Test
	public void testObjectDateTimeGreaterThanOrEqual2() {
		assertTrue(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", null)));
		assertTrue(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-17", null)));
	}

	@Test
	public void testNullObjectDateTimeGreaterThanOrEqual2() {
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(null));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateGreaterThanOrEqual(ObjectFrom<String>::getSource, (String) null, "yyyy-MM-dd ").test(new ObjectFrom<String>(null, null)));
	}

	//// dateLessThan(Function, Function, pattern)

	@Test
	public void testObjectDateTimeLessThan() {
		assertTrue(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", "2019-09-19")));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", "2019-09-19")));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-20", "2019-09-19")));
	}

	@Test
	public void testNullObjectDateTimeLessThan() {
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(null));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateLessThan(Function, String, pattern)

	@Test
	public void testObjectDateTimeLessThan2() {
		assertTrue(dateLessThan(ObjectFrom<String>::getSource, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", "2019-09-19")));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", "2019-09-19")));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-20", "2019-09-19")));
	}

	@Test
	public void testNullObjectDateTimeLessThan2() {
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(null));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateLessThan(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateLessThanOrEqual(Function, Function, pattern)

	@Test
	public void testObjectDateTimeLessThanOrEqual() {
		assertTrue(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", "2019-09-19")));
		assertTrue(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", "2019-09-19")));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-20", "2019-09-19")));
	}

	@Test
	public void testNullObjectDateTimeLessThanOrEqual() {
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(null));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, ObjectFrom::getTarget, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateLessThanOrEqual(Function, String, pattern)

	@Test
	public void testObjectDateTimeLessThanOrEqual2() {
		assertTrue(dateLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", "2019-09-19")));
		assertTrue(dateLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", "2019-09-19")));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-20", "2019-09-19")));
	}

	@Test
	public void testNullObjectDateTimeLessThanOrEqual2() {
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(null));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, "2019-09-18", YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-18")));
		assertFalse(dateLessThanOrEqual(ObjectFrom<String>::getSource, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
	}

	//// dateBetween(Function, String, String, pattern)

	@Test
	public void testNullObjectDateTimeBetween() {
		assertFalse(dateBetween(ObjectFrom<String>::getSource, "2019-09-19", (String) null, (String) null).test(null));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, "2019-09-19", (String) null).test(new ObjectFrom<String>("2019-09-19", null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, (String) null, (String) null).test(new ObjectFrom<String>(null, null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, "2019-09-19", "2019-09-19", null).test(new ObjectFrom<String>("2019-09-19", null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-19")));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, "2019-09-19", "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>(null, null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-19")));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, null, (String) null, YYYY_MM_DD).test(new ObjectFrom<String>(null, "2019-09-19")));
	}

	@Test
	public void testObjectDateTimeBetween() {
		assertTrue(dateBetween(ObjectFrom<String>::getSource, "2019-09-19", "2019-09-19", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", null)));
		assertTrue(dateBetween(ObjectFrom<String>::getSource, "2019-09-18", "2019-09-20", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-20", null)));
		assertTrue(dateBetween(ObjectFrom<String>::getSource, "2019-09-18", "2019-09-20", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-19", null)));
		assertTrue(dateBetween(ObjectFrom<String>::getSource, "2019-09-18", "2019-09-20", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-18", null)));
		assertFalse(dateBetween(ObjectFrom<String>::getSource, "2019-09-18", "2019-09-20", YYYY_MM_DD).test(new ObjectFrom<String>("2019-09-17", null)));
	}

	//// multi thread test

	@Test
	public void testDatePredicateMultiThreadMustBeTrue() throws InterruptedException {

		final int CONCURRENT_RUNNABLE = 100000;

		final Collection<Boolean> resultsOne = new ConcurrentLinkedQueue<>();

		final ExecutorService executorService = Executors.newFixedThreadPool(10);

		for (int i = 0; i < CONCURRENT_RUNNABLE; i++) {
			executorService.submit(new Runnable() {
				@Override
				public void run() {
					assertThatCode(() -> {
						resultsOne.add(dateBetween("2018-06-22T10:00:00", "2018-06-22T10:00:00", "yyyy-MM-dd'T'HH:mm:ss").test("2018-06-22T10:00:00"));
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

	//// dateIsAfterToday()

	@Test
	public void testDateIsAfterToday() {
		assertFalse(dateIsAfterToday().test(LocalDate.now()));
		assertFalse(dateIsAfterToday().test(LocalDate.now().minusDays(1)));
		assertFalse(dateIsAfterToday().test(LocalDate.now().minusMonths(1)));
		assertFalse(dateIsAfterToday().test(LocalDate.now().minusYears(1)));
		assertTrue(dateIsAfterToday().test(LocalDate.now().plusDays(1)));
		assertTrue(dateIsAfterToday().test(LocalDate.now().plusMonths(1)));
		assertTrue(dateIsAfterToday().test(LocalDate.now().plusYears(1)));
	}

	@Test
	public void testNullObjectDateIsAfterToday() {
		assertFalse(dateIsAfterToday().test((LocalDate) null));
	}

	//// dateIsBeforeToday()

	@Test
	public void testDateIsBeforeToday() {
		assertFalse(dateIsBeforeToday().test(LocalDate.now()));
		assertTrue(dateIsBeforeToday().test(LocalDate.now().minusDays(1)));
		assertTrue(dateIsBeforeToday().test(LocalDate.now().minusMonths(1)));
		assertTrue(dateIsBeforeToday().test(LocalDate.now().minusYears(1)));
		assertFalse(dateIsBeforeToday().test(LocalDate.now().plusDays(1)));
		assertFalse(dateIsBeforeToday().test(LocalDate.now().plusMonths(1)));
		assertFalse(dateIsBeforeToday().test(LocalDate.now().plusYears(1)));
	}

	@Test
	public void testNullObjectDateIsBeforeToday() {
		assertFalse(dateIsBeforeToday().test((LocalDate) null));
	}

	//// dateIsAfterThan(Function)

	@Test
	public void testDateIsAfterThan() {
		TestClass testClass = new TestClass();

		assertFalse(dateIsAfterThan(TestClass::getSource, TestClass::getTarget).test(testClass));

		testClass.setTarget(LocalDate.now().plusDays(1));
		assertFalse(dateIsAfterThan(TestClass::getSource, TestClass::getTarget).test(testClass));

		testClass.setTarget(LocalDate.now().minusDays(1));
		assertTrue(dateIsAfterThan(TestClass::getSource, TestClass::getTarget).test(testClass));
	}

	//// dateIsBeforeThan(Function)

	@Test
	public void testDateIsBeforeThan() {
		TestClass testClass = new TestClass();

		assertFalse(dateIsBeforeThan(TestClass::getSource, TestClass::getTarget).test(testClass));

		testClass.setTarget(LocalDate.now().plusDays(1));
		assertTrue(dateIsBeforeThan(TestClass::getSource, TestClass::getTarget).test(testClass));

		testClass.setTarget(LocalDate.now().minusDays(1));
		assertFalse(dateIsBeforeThan(TestClass::getSource, TestClass::getTarget).test(testClass));
	}



	class TestClass {
		private LocalDate source = LocalDate.now();
		private LocalDate target = LocalDate.now();

		public LocalDate getSource() {
			return source;
		}

		public void setSource(LocalDate source) {
			this.source = source;
		}

		public LocalDate getTarget() {
			return target;
		}

		public void setTarget(LocalDate target) {
			this.target = target;
		}
	}

}
