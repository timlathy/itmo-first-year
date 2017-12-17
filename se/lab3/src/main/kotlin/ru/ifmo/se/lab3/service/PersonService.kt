package ru.ifmo.se.lab3.service

import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.Person
import ru.ifmo.se.lab3.repository.PersonRepository

@Service
class PersonService(private val repo: PersonRepository) {
  fun createPerson(dto: Person.Dto) =
    repo.save(Person(name = dto.name))
}
