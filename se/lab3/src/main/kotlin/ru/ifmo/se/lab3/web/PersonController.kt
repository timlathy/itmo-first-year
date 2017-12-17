package ru.ifmo.se.lab3.web

import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.Person
import ru.ifmo.se.lab3.repository.PersonRepository
import ru.ifmo.se.lab3.service.PersonService

@RestController
class PersonController(private val repo: PersonRepository,
                       private val personService: PersonService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @PostMapping("/people")
  fun createPerson(@Valid @RequestBody dto: Person.Dto) =
    personService.createPerson(dto)

  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/people/{name}")
  fun findByName(@PathVariable name: String) = repo.findByName(name)
}
