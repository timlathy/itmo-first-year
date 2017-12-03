package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class CharacterController(private val repository: CharacterRepository) {
  @GetMapping("/characters/{name}")
  fun findByName(@PathVariable name: String) = repository.findByName(name)
}
