package ru.ifmo.se.lab3

import javax.validation.ValidationException
import java.time.LocalDateTime

import org.springframework.stereotype.Service

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
