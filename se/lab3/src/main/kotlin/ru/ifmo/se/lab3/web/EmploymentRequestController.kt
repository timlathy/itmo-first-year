package ru.ifmo.se.lab3.web

import java.security.Principal
import javax.validation.Valid
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.EmploymentRequest
import ru.ifmo.se.lab3.repository.EmploymentRequestRepository
import ru.ifmo.se.lab3.service.EmploymentRequestService

@RestController
class EmploymentRequestController(private val repo: EmploymentRequestRepository,
                                  private val service: EmploymentRequestService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/employment/requests/{applicantName}")
  fun readRequests(applicantName: String) =
    repo.findByApplicantName(applicantName)
  
  @PreAuthorize("hasRole('ROLE_SURV_DEVICE')")
  @GetMapping("/employment/requests")
  fun readRequests(authorizedApplicant: Principal) =
    repo.findByApplicantName(authorizedApplicant.getName())
   
  @PreAuthorize("hasRole('ROLE_SURV_DEVICE')")
  @PostMapping("/employment/requests")
  fun createRequest(authorizedApplicant: Principal) =
    service.createRequest(authorizedApplicant.getName())
  
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @PatchMapping("/employment/requests/{id}")
  fun updateRequest(@PathVariable id: Long,
                    @Valid @RequestBody dto: EmploymentRequest.UpdateDto) =
    service.updateRequest(id, dto)
}

