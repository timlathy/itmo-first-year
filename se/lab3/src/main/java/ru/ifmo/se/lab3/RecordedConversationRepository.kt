package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository

interface RecordedConversationRepository : CrudRepository<RecordedConversation, Long> {
  fun findByParticipants(participant: Person): Iterable<RecordedConversation>
}
