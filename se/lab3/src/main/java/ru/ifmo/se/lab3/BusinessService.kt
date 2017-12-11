package ru.ifmo.se.lab3

import javax.validation.ValidationException
import java.time.LocalDateTime

import org.springframework.stereotype.Service

@Service
class BusinessService(private val repo: BusinessRepository,
                      private val personRepo: PersonRepository) {
  fun createBusiness(dto: Business.Dto) =
    repo.save(Business(
      name = dto.name,
      owner = personRepo.findByName(dto.ownerName)))
}
