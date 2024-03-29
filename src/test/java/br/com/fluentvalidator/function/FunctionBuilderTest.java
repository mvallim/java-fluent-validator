/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.com.fluentvalidator.function;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.function.Function;

import org.junit.Test;

import br.com.fluentvalidator.function.FunctionBuilderTest.FunctionalTest.FunctionalTestInner01;
import br.com.fluentvalidator.function.FunctionBuilderTest.FunctionalTest.FunctionalTestInner01.FunctionalTestInner02;

public class FunctionBuilderTest {

  @Test
  public void testSuccessAndThen() {

    final FunctionalTest functionalTest = new FunctionalTest();

    final FunctionalTestInner01 functionalTestInner01 = new FunctionalTestInner01();

    final FunctionalTestInner02 functionalTestInner02 = new FunctionalTestInner02();

    functionalTestInner02.setValue(1);

    functionalTestInner01.setFunctionalTestInner02(functionalTestInner02);

    functionalTest.setFunctionalTestInner(functionalTestInner01);

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTest::getFunctionalTestInner01).andThen(FunctionalTestInner01::getFunctionalTestInner02).andThen(FunctionalTestInner02::getValue);

    assertThat(function.apply(functionalTest), equalTo(1));

  }

  @Test
  public void testSuccessCompose() {

    final FunctionalTest functionalTest = new FunctionalTest();

    final FunctionalTestInner01 functionalTestInner01 = new FunctionalTestInner01();

    final FunctionalTestInner02 functionalTestInner02 = new FunctionalTestInner02();

    functionalTestInner02.setValue(1);

    functionalTestInner01.setFunctionalTestInner02(functionalTestInner02);

    functionalTest.setFunctionalTestInner(functionalTestInner01);

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTestInner02::getValue).compose(FunctionalTestInner01::getFunctionalTestInner02).compose(FunctionalTest::getFunctionalTestInner01);

    assertThat(function.apply(functionalTest), equalTo(1));

  }

  @Test
  public void testSuccessAndThenWhenNullValueInChain01() {

    final FunctionalTest functionalTest = new FunctionalTest();

    final FunctionalTestInner01 functionalTestInner01 = new FunctionalTestInner01();

    functionalTest.setFunctionalTestInner(functionalTestInner01);

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTest::getFunctionalTestInner01).andThen(FunctionalTestInner01::getFunctionalTestInner02).andThen(FunctionalTestInner02::getValue);

    assertThat(function.apply(functionalTest), nullValue());

  }

  @Test
  public void testSuccessAndThenWhenNullValueInChain02() {

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTest::getFunctionalTestInner01).andThen(FunctionalTestInner01::getFunctionalTestInner02).andThen(FunctionalTestInner02::getValue);

    assertThat(function.apply(null), nullValue());

  }

  @Test
  public void testSuccessAndThenWhenNullValueInChain03() {

    final FunctionalTest functionalTest = new FunctionalTest();

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTest::getFunctionalTestInner01).andThen(FunctionalTestInner01::getFunctionalTestInner02).andThen(FunctionalTestInner02::getValue);

    assertThat(function.apply(functionalTest), nullValue());

  }

  @Test
  public void testSuccessComposeWhenNullValueInChain01() {

    final FunctionalTest functionalTest = new FunctionalTest();

    final FunctionalTestInner01 functionalTestInner01 = new FunctionalTestInner01();

    functionalTest.setFunctionalTestInner(functionalTestInner01);

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTestInner02::getValue).compose(FunctionalTestInner01::getFunctionalTestInner02).compose(FunctionalTest::getFunctionalTestInner01);

    assertThat(function.apply(functionalTest), nullValue());

  }

  @Test
  public void testSuccessComposeWhenNullValueInChain02() {

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTestInner02::getValue).compose(FunctionalTestInner01::getFunctionalTestInner02).compose(FunctionalTest::getFunctionalTestInner01);

    assertThat(function.apply(null), nullValue());

  }

  @Test
  public void testSuccessComposeWhenNullValueInChain03() {

    final FunctionalTest functionalTest = new FunctionalTest();

    final Function<FunctionalTest, Integer> function = FunctionBuilder.of(FunctionalTestInner02::getValue).compose(FunctionalTestInner01::getFunctionalTestInner02).compose(FunctionalTest::getFunctionalTestInner01);

    assertThat(function.apply(functionalTest), nullValue());

  }

  static class FunctionalTest {

    private FunctionalTestInner01 functionalTestInner01;

    public FunctionalTestInner01 getFunctionalTestInner01() {
      return functionalTestInner01;
    }

    public void setFunctionalTestInner(final FunctionalTestInner01 functionalTestInner01) {
      this.functionalTestInner01 = functionalTestInner01;
    }

    static class FunctionalTestInner01 {

      private FunctionalTestInner02 functionalTestInner02;

      public FunctionalTestInner02 getFunctionalTestInner02() {
        return functionalTestInner02;
      }

      public void setFunctionalTestInner02(final FunctionalTestInner02 functionalTestInner02) {
        this.functionalTestInner02 = functionalTestInner02;
      }

      static class FunctionalTestInner02 {

        private Integer integer;

        public Integer getValue() {
          return integer;
        }

        public void setValue(final Integer value) {
          integer = value;
        }

      }

    }

  }

}
