package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class ActionController(private val repo: ActionRepository,
                       private val personRepo: PersonRepository) {
  @GetMapping("/actions/{actorName}")
  fun findByActor(@PathVariable actorName: String) =
    repo.findByActor(personRepo.findByName(actorName))
}

