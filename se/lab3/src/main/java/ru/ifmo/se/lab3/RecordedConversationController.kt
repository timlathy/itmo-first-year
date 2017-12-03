package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class RecordedConversationController(private val repo: RecordedConversationRepository,
                                     private val personRepo: PersonRepository) {
  @GetMapping("/conversations/{participantName}")
  fun findByParticipant(@PathVariable participantName: String) =
    repo.findByParticipants(personRepo.findByName(participantName))
}
