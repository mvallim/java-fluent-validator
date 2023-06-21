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

package br.com.fluentvalidator.exception;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.Test;

public class ValidationExceptionTest {

    @Test
    public void testSuccess() {

        assertThatThrownBy(() -> {
            throw ValidationException.create(ValidationSampleException.class);
        }).isInstanceOf(ValidationSampleException.class);

    }

    @Test
    public void testFail() {

        assertThatThrownBy(() -> {
            throw ValidationException.create(ValidationSampleInvalidException.class);
        }).isInstanceOf(RuntimeException.class);

    }

}
