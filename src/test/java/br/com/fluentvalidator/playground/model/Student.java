/*
 * Copyright 2023 the original author or authors.
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

package br.com.fluentvalidator.playground.model;

public class Student {
  private final String enrolmentId;
  private final String id;

  public Student(final Builder builder) {
    enrolmentId = builder.enrolmentId;
    id = builder.id;
  }

  public String getId() {
    return id;
  }

  public String getEnrolmentId() {
    return enrolmentId;
  }

  public static class Builder {

    private String enrolmentId;
    private String id;

    public static Builder newInstance() {
      return new Student.Builder();
    }

    public Builder setEnrolmentId(final String enrolmentId) {
      this.enrolmentId = enrolmentId;
      return this;
    }

    public Builder setId(final String id) {
      this.id = id;
      return this;
    }

    public Student build() {
      return new Student(this);
    }

  }

}
