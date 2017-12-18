package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository

import ru.ifmo.se.lab3.domain.EmploymentRequest

interface EmploymentRequestRepository : CrudRepository<EmploymentRequest, Long> {
  fun findByApplicantName(applicantName: String): EmploymentRequest

  fun existsByApplicantNameAndStatus(applicantName: String,
    status: EmploymentRequest.Status): Boolean
}
