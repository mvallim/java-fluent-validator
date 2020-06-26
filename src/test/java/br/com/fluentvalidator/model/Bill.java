package br.com.fluentvalidator.model;

import java.time.LocalDate;

public class Bill {

  private String description;

  private Float value;

  private LocalDate dueDate;

  public Bill(String description, Float value, LocalDate dueDate) {
    this.description = description;
    this.value = value;
    this.dueDate = dueDate;
  }

  public String getDescription() {
    return description;
  }

  public Float getValue() {
    return value;
  }

  public LocalDate getDueDate() {
    return dueDate;
  }
}
