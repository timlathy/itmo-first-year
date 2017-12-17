package ru.ifmo.se.lab3.web

import java.security.Principal
import javax.validation.Valid
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.RecordedConversation
import ru.ifmo.se.lab3.repository.RecordedConversationRepository
import ru.ifmo.se.lab3.repository.PersonRepository
import ru.ifmo.se.lab3.service.RecordedConversationService

@RestController
class RecordedConversationController(private val repo: RecordedConversationRepository,
                                     private val personRepo: PersonRepository,
                                     private val service: RecordedConversationService) {
  @PreAuthorize("hasRole('ROLE_SURV_DEVICE')")
  @PostMapping("/conversations")
  fun createConversation(submitter: Principal,
                         @Valid @RequestBody dto: RecordedConversation.Dto) =
    service.createConversation(submitter.getName(), dto)

  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/conversations/{participantName}")
  fun findByParticipant(@PathVariable participantName: String) =
    repo.findByParticipants(personRepo.findByName(participantName))
}
