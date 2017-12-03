package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class PersonController(private val repo: PersonRepository) {
  @GetMapping("/people/{name}")
  fun findByName(@PathVariable name: String) = repo.findByName(name)
}
