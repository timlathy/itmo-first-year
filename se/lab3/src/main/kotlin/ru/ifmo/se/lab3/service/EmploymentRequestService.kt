package ru.ifmo.se.lab3.service

import java.time.LocalDateTime
import javax.validation.ValidationException
import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.EmploymentRequest
import ru.ifmo.se.lab3.repository.EmploymentRequestRepository
import ru.ifmo.se.lab3.repository.PersonRepository

@Service
class EmploymentRequestService(private val repo: EmploymentRequestRepository,
                               private val personRepo: PersonRepository) {
  fun createRequest(applicantName: String): EmploymentRequest {
    /* TODO: Add a database constraint (partial index?) */
    if (repo.existsByApplicantNameAndStatus(applicantName,
          EmploymentRequest.Status.PROCESSING))
      throw ValidationException("You have already submitted a request that is currently being processed.")

    return repo.save(EmploymentRequest(
      applicant = personRepo.findByName(applicantName),
      date = LocalDateTime.now()))
  }

  fun updateRequest(id: Long, dto: EmploymentRequest.UpdateDto) =
    repo.findById(id).get().apply {
      status = dto.status
      details = dto.details

      repo.save(this)
    }
}
