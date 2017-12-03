package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class RemarkController(private val repo: RemarkRepository,
                       private val characterRepo: CharacterRepository) {
  @GetMapping("/remarks/{speakerName}")
  fun findBySpeaker(@PathVariable speakerName: String) =
    repo.findBySpeaker(characterRepo.findByName(speakerName))
}

