package ru.ifmo.se.lab3.domain

import java.time.LocalDateTime
import javax.persistence.*
import com.fasterxml.jackson.databind.annotation.JsonSerialize

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
abstract class Action(
  @ManyToOne
  @JoinColumn(name="actor_id")
  @JsonSerialize(using = Person.ToNameStringSerializer::class)
  val actor: Person,

  val date: LocalDateTime,

  @Id @GeneratedValue
  val id: Long = -1)
