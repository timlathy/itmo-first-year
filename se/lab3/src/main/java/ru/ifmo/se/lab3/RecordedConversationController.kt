package ru.ifmo.se.lab3

import java.security.Principal
import javax.validation.Valid
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

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
