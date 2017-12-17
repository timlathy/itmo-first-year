package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository

import ru.ifmo.se.lab3.domain.RecordedConversation
import ru.ifmo.se.lab3.domain.Person

interface RecordedConversationRepository : CrudRepository<RecordedConversation, Long> {
  fun findByParticipants(participant: Person): Iterable<RecordedConversation>
}
