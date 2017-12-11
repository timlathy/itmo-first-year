package ru.ifmo.se.lab3

import java.security.Principal;
import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

@RestController
class ActionController(private val repo: ActionRepository,
                       private val personRepo: PersonRepository,
                       private val actionService: ActionService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/actions/{actorName}")
  fun readByActor(@PathVariable actorName: String) =
    repo.findByActorName(actorName)

  @PreAuthorize("hasRole('ROLE_SURV_DEVICE')")
  @PostMapping("/actions/locationChanges")
  fun createLocationChange(authorizedActor: Principal,
                           @Valid @RequestBody action: LocationChangeAction.Dto) =
    actionService.createLocationChangeAction(loadAuthorizedPerson(authorizedActor), action)

  private fun loadAuthorizedPerson(principal: Principal) =
    personRepo.findByName(principal.getName())
}

