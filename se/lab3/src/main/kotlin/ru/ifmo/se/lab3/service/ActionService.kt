package ru.ifmo.se.lab3.service

import java.time.LocalDateTime
import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.LocationChangeAction
import ru.ifmo.se.lab3.domain.Person
import ru.ifmo.se.lab3.repository.ActionRepository
import ru.ifmo.se.lab3.repository.PersonRepository

@Service
class ActionService(private val repo: ActionRepository,
                    private val personRepo: PersonRepository) {
  fun createLocationChangeAction(actorName: String,
                                 dto: LocationChangeAction.Dto) =
    repo.save(LocationChangeAction(
      actor = personRepo.findByName(actorName),
      date = LocalDateTime.now(),
      newLocation = dto.newLocation,
      means = dto.means))
}
