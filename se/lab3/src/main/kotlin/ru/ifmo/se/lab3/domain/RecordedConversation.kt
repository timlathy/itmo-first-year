package ru.ifmo.se.lab3.domain

import java.time.LocalDateTime
import javax.persistence.*
import javax.validation.constraints.*
import com.fasterxml.jackson.databind.annotation.JsonSerialize

@Entity
data class RecordedConversation(
  @ManyToMany
  @JoinTable(name = "recorded_conversation_participants")
  @JsonSerialize(contentUsing = Person.ToNameStringSerializer::class)
  val participants: Set<Person>,

  val recognizedContent: String,

  val date: LocalDateTime,

  @Id @GeneratedValue
  val id: Long = -1) {

  /**
   * A DTO representing a new RecordedConversation record.
   *
   * Owners are instantiated from names by
   * [RecordedConversationService], which is also
   * responsible for converting the DTO to an entity bean.
   *
   * IMPORTANT: Note that the person whose device is submitting
   * the data is automatically appended to the conversation participants.
   */
  data class Dto(
    @NotNull
    val participantNames: Set<String>,

    @NotBlank
    val content: String,

    @NotNull
    val date: LocalDateTime)
}
