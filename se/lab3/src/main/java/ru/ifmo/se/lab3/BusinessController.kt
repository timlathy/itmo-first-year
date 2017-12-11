package ru.ifmo.se.lab3

import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

@RestController
class BusinessController(private val repo: BusinessRepository,
                         private val businessService: BusinessService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/businesses")
  fun readAll() = repo.findAll()

  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @PostMapping("/businesses")
  fun createBusiness(@Valid @RequestBody business: Business.Dto)
    = businessService.createBusiness(business)
}

