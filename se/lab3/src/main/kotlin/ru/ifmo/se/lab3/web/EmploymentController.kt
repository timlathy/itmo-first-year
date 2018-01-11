package ru.ifmo.se.lab3.web

import java.security.Principal
import javax.validation.Valid
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.Employment
import ru.ifmo.se.lab3.repository.EmploymentRepository
import ru.ifmo.se.lab3.service.EmploymentService

@RestController
class EmploymentController(private val repo: EmploymentRepository,
                           private val service: EmploymentService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/employment/at/{employerName}")
  fun readByEmployer(@PathVariable employerName: String) =
    repo.findByEmployerName(employerName)
  
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @PostMapping("/employment")
  fun createEmployment(@Valid @RequestBody dto: Employment.Dto) =
    service.create(dto)
  
  // TODO: update/delete
}

