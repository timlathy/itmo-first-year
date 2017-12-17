package ru.ifmo.se.lab3.service

import java.time.LocalDateTime
import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.LocationChangeAction
import ru.ifmo.se.lab3.domain.Person
import ru.ifmo.se.lab3.repository.ActionRepository

@Service
class ActionService(private val repo: ActionRepository) {
  fun createLocationChangeAction(actor: Person,
                                 dto: LocationChangeAction.Dto) =
    repo.save(LocationChangeAction(
      actor = actor,
      date = LocalDateTime.now(),
      newLocation = dto.newLocation,
      means = dto.means))
}
