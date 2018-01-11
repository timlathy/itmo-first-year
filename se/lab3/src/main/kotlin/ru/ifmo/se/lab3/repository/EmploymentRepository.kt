package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository

import ru.ifmo.se.lab3.domain.Employment

interface EmploymentRepository : CrudRepository<Employment, Long> {
  fun findByEmployerName(employerName: String): Iterable<Employment>

  fun findByEmployeeName(employeeName: String): Iterable<Employment>
}
