package ru.ifmo.se.lab7.server

import javax.validation.ConstraintViolation
import javax.validation.Validation

class ObjectValidator<E> {
  private val validator = Validation.buildDefaultValidatorFactory().validator

  fun findViolations(item: E): List<String>? =
    validator
      .validate(item)
      .takeIf(Set<ConstraintViolation<E>>::isNotEmpty)
      ?.map { it.message }
      ?.sorted()

  fun findViolationsAsString(item: E): String? =
    findViolations(item)?.joinToString(", ")
}
