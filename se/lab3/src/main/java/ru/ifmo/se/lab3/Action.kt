package ru.ifmo.se.lab3

import javax.persistence.*
import java.time.LocalDateTime

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
abstract class Action(
  @ManyToOne
  @JoinColumn(name="actor_id")
  val actor: Person,

  val date: LocalDateTime,

  @Id @GeneratedValue
  val id: Long = -1)
