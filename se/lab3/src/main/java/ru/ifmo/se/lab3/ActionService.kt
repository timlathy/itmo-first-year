package ru.ifmo.se.lab3

import org.springframework.stereotype.Service

import java.time.LocalDateTime

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
