package ru.ifmo.se.lab3.service

import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.Employment
import ru.ifmo.se.lab3.repository.EmploymentRepository
import ru.ifmo.se.lab3.repository.PersonRepository
import ru.ifmo.se.lab3.repository.BusinessRepository

@Service
class EmploymentService(private val repo: EmploymentRepository,
                        private val personRepo: PersonRepository,
                        private val businessRepo: BusinessRepository) {
  fun create(dto: Employment.Dto): Employment =
    repo.save(Employment(
      employee = personRepo.findByName(dto.employeeName),
      employer = businessRepo.findByName(dto.employerName),
      title = dto.title))
}
