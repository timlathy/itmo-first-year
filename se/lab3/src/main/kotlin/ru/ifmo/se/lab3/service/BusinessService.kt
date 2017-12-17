package ru.ifmo.se.lab3.service

import javax.validation.ValidationException
import java.time.LocalDateTime
import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.Business
import ru.ifmo.se.lab3.repository.BusinessRepository
import ru.ifmo.se.lab3.repository.PersonRepository


@Service
class BusinessService(private val repo: BusinessRepository,
                      private val personRepo: PersonRepository) {
  fun createBusiness(dto: Business.Dto) =
    repo.save(Business(
      name = dto.name,
      owner = personRepo.findByName(dto.ownerName)))
}
