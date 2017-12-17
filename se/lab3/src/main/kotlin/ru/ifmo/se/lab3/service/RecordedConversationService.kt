package ru.ifmo.se.lab3.service

import java.time.LocalDateTime
import javax.validation.ValidationException
import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.RecordedConversation
import ru.ifmo.se.lab3.domain.Person
import ru.ifmo.se.lab3.repository.RecordedConversationRepository
import ru.ifmo.se.lab3.repository.PersonRepository

@Service
class RecordedConversationService(private val repo: RecordedConversationRepository,
                                  private val personRepo: PersonRepository) {
  fun createConversation(submitterName: String, dto: RecordedConversation.Dto) {
    val participants = dto.participantNames.plus(submitterName).map(personRepo::findByName).toSet()

    repo.save(RecordedConversation(
      participants = participants,
      recognizedContent = dto.content,
      date = dto.date))
  }
}
