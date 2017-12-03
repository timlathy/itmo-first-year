package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
data class RecordedConversation(
  @ManyToMany
  @JoinTable(name = "recorded_conversation_participants")
  val participants: Set<Person>,

  val recognizedContent: String,

  @Id @GeneratedValue
  val id: Long = -1)
