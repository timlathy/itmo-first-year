package ru.ifmo.se.lab3

import org.springframework.stereotype.Service

@Service
class PersonService(private val repo: PersonRepository) {
  fun createPerson(dto: Person.Dto) =
    repo.save(Person(name = dto.name))
}
