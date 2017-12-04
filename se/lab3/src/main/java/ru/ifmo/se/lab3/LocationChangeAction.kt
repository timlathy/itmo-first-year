package ru.ifmo.se.lab3

import javax.persistence.*
import java.time.LocalDateTime

@Entity
class LocationChangeAction(
  actor: Person,

  date: LocalDateTime,
  
  val newLocation: String,
  
  @Enumerated(EnumType.STRING)
  val means: TransportationMeans) : Action(actor, date)
