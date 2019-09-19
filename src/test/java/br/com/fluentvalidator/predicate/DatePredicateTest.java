package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.predicate.DatePredicate.*;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class DatePredicateTest {
	
	@Test
	public void testNullDateEqualTo() {
		assertFalse(dateEqualTo(null, null).test(null));
		assertFalse(dateEqualTo(null, null).test("2019-09-19"));
		assertFalse(dateEqualTo(null, "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateEqualTo(null, "YYYY-MM-DD").test(null));
		assertFalse(dateEqualTo("2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateEqualTo("2019-09-19", null).test(null));
		assertFalse(dateEqualTo("2019-09-19", null).test("2019-09-19"));
	}
	
	@Test
	public void testDateEqualToInvalid() {
		assertThatThrownBy(() -> dateEqualTo("2019-09-19", "yyyy-MM-dd").test("2019/09-19")).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> dateEqualTo("2019-09-19", "yyyy/MM-dd").test("2019-09-19")).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> dateEqualTo("2019/09-19", "yyyy-MM-dd").test("2019-09-19")).isInstanceOf(IllegalArgumentException.class);
	}

	@Test
	public void testDateEqualTo() {
		assertTrue(dateEqualTo("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
		assertFalse(dateEqualTo("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
	}

	@Test
	public void testNullDateGreaterThan() {
		assertFalse(dateGreaterThan(null, null).test(null));
		assertFalse(dateGreaterThan(null, null).test("2019-09-19"));
		assertFalse(dateGreaterThan(null, "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateGreaterThan(null, "YYYY-MM-DD").test(null));
		assertFalse(dateGreaterThan("2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateGreaterThan("2019-09-19", null).test(null));
		assertFalse(dateGreaterThan("2019-09-19", null).test("2019-09-19"));
	}
	
	@Test
	public void testDateGreaterThanInvalid() {
		assertThatThrownBy(() -> dateGreaterThan("2019-09-19", "yyyy-MM-dd").test("2019/09-19")).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> dateGreaterThan("2019-09-19", "yyyy/MM-dd").test("2019-09-19")).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> dateGreaterThan("2019/09-19", "yyyy-MM-dd").test("2019-09-19")).isInstanceOf(IllegalArgumentException.class);
	}

	@Test
	public void testDateGreaterThan() {
		assertTrue(dateGreaterThan("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
		assertFalse(dateGreaterThan("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
	}
	
	@Test
	public void testNullDateLessThanThan() {
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
		assertThatThrownBy(() -> dateLessThan("2019-09-19", "yyyy-MM-dd").test("2019/09-19")).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> dateLessThan("2019-09-19", "yyyy/MM-dd").test("2019-09-19")).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> dateLessThan("2019/09-19", "yyyy-MM-dd").test("2019-09-19")).isInstanceOf(IllegalArgumentException.class);
	}

	@Test
	public void testDateLessThan() {
		assertTrue(dateLessThan("2019-09-19", "yyyy-MM-dd").test("2019-09-18"));
		assertFalse(dateLessThan("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
	}
	
	@Test
	public void testNullDateGreaterThanOrEqual() {
		assertFalse(dateGreaterThanOrEqual(null, null).test(null));
		assertFalse(dateGreaterThanOrEqual(null, null).test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual(null, "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual(null, "YYYY-MM-DD").test(null));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", null).test(null));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateGreaterThanOrEqual() {
		assertTrue(dateGreaterThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
		assertTrue(dateGreaterThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
		assertFalse(dateGreaterThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-18"));
	}
	
	@Test
	public void testNullDateLessThanOrEqual() {
		assertFalse(dateLessThanOrEqual(null, null).test(null));
		assertFalse(dateLessThanOrEqual(null, null).test("2019-09-19"));
		assertFalse(dateLessThanOrEqual(null, "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateLessThanOrEqual(null, "YYYY-MM-DD").test(null));
		assertFalse(dateLessThanOrEqual("2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateLessThanOrEqual("2019-09-19", null).test(null));
		assertFalse(dateLessThanOrEqual("2019-09-19", null).test("2019-09-19"));
	}

	@Test
	public void testDateLessThanOrEqual() {
		assertTrue(dateLessThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-18"));
		assertTrue(dateLessThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
		assertFalse(dateLessThanOrEqual("2019-09-19", "yyyy-MM-dd").test("2019-09-20"));
	}
	
	@Test
	public void testNullDateBetween() {
		assertFalse(dateBetween("2019-09-19", null, null).test(null));
		assertFalse(dateBetween(null, "2019-09-19", null).test(null));
		assertFalse(dateBetween(null, null, "YYYY-MM-DD").test(null));
		assertFalse(dateBetween(null, null, null).test("2019-09-19"));
		assertFalse(dateBetween("2019-09-19", "2019-09-19", null).test(null));
		assertFalse(dateBetween(null, "2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateBetween(null, null, "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateBetween("2019-09-19", "2019-09-19", "YYYY-MM-DD").test(null));
		assertFalse(dateBetween(null, "2019-09-19", "YYYY-MM-DD").test("2019-09-19"));
		assertFalse(dateBetween(null, null, "YYYY-MM-DD").test("2019-09-19"));
	}

	@Test
	public void testDateBetween() {
		assertTrue(dateBetween("2019-09-19", "2019-09-19", "yyyy-MM-dd").test("2019-09-19"));
		assertTrue(dateBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-20"));
		assertTrue(dateBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-18"));
		assertTrue(dateBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-19"));
		assertFalse(dateBetween("2019-09-18", "2019-09-20", "yyyy-MM-dd").test("2019-09-17"));
	}

}
