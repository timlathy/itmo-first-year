package ru.ifmo.se.lab3

import javax.persistence.*
import java.time.LocalDateTime

@Entity
data class RecordedConversation(
  @ManyToMany
  @JoinTable(name = "recorded_conversation_participants")
  val participants: Set<Person>,

  val recognizedContent: String,

  val date: LocalDateTime,

  @Id @GeneratedValue
  val id: Long = -1)
