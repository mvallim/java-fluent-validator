package br.com.fluentvalidator.predicate;

import static br.com.fluentvalidator.function.FunctionBuilder.of;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.MapPredicate.mapGet;
import static br.com.fluentvalidator.predicate.StringPredicate.isAlpha;
import static br.com.fluentvalidator.predicate.StringPredicate.isNumber;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;
import static br.com.fluentvalidator.predicate.StringPredicate.stringSize;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

public class MapPredicateTest {

  private final Map<String, String> map = new HashMap<>();

  @Before
  public void before() {
    map.clear();
    map.put("a", "123456");
    map.put("b", "aeiou");
    map.put("c", null);
  }

  @Test
  public void testMapGetKPredicateOfV() {
    assertThat(mapGet(of(x -> "a"), stringSize(6)).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "a"), isNumber()).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "a"), not(stringEmptyOrNull())).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "b"), stringSize(5)).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "b"), isAlpha()).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "b"), not(stringEmptyOrNull())).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "c"), stringEmptyOrNull()).test(map), equalTo(true));
    assertThat(mapGet(of(x -> "c"), not(stringEmptyOrNull())).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>mapGet(of(x -> "a"), null).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>mapGet(null, not(stringEmptyOrNull())).test(map), equalTo(false));
  }

  @Test
  public void testMapGetFunctionOfTKPredicateOfV() {
    assertThat(mapGet("a", stringSize(6)).test(map), equalTo(true));
    assertThat(mapGet("a", isNumber()).test(map), equalTo(true));
    assertThat(mapGet("a", not(stringEmptyOrNull())).test(map), equalTo(true));
    assertThat(mapGet("b", stringSize(5)).test(map), equalTo(true));
    assertThat(mapGet("b", isAlpha()).test(map), equalTo(true));
    assertThat(mapGet("b", not(stringEmptyOrNull())).test(map), equalTo(true));
    assertThat(mapGet("c", stringEmptyOrNull()).test(map), equalTo(true));
    assertThat(mapGet("c", not(stringEmptyOrNull())).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>mapGet("a", null).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>mapGet(null, not(stringEmptyOrNull())).test(map), equalTo(false));
  }

  @Test
  public void testContainsKeyK() {
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey("a").test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey("b").test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey("c").test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey("d").test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(null).test(map), equalTo(false));
  }

  @Test
  public void testContainsKeyFunctionOfTK() {
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(of(x -> "a")).test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(of(x -> "b")).test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(of(x -> "c")).test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(of(x -> "d")).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(null).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsKey(null).test(null), equalTo(false));
  }

  @Test
  public void testContainsValueV() {
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue("aeiou").test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue("123456").test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue("d").test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(null).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(null).test(null), equalTo(false));
  }

  @Test
  public void testContainsValueFunctionOfTV() {
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(of(x -> "aeiou")).test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(of(x -> "123456")).test(map), equalTo(true));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(of(x -> "d")).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(null).test(map), equalTo(false));
    assertThat(MapPredicate.<String, String, Map<String, String>>containsValue(null).test(null), equalTo(false));
  }

}
