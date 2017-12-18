package ru.ifmo.se.lab3.web

import java.security.Principal
import javax.validation.Valid
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.LocationChangeAction
import ru.ifmo.se.lab3.repository.ActionRepository
import ru.ifmo.se.lab3.service.ActionService

@RestController
class ActionController(private val repo: ActionRepository,
                       private val actionService: ActionService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/actions/{actorName}")
  fun readByActor(@PathVariable actorName: String) =
    repo.findByActorName(actorName)

  @PreAuthorize("hasRole('ROLE_SURV_DEVICE')")
  @PostMapping("/actions/locationChanges")
  fun createLocationChange(authorizedActor: Principal,
                           @Valid @RequestBody action: LocationChangeAction.Dto) =
    actionService.createLocationChangeAction(authorizedActor.getName(), action)
}

