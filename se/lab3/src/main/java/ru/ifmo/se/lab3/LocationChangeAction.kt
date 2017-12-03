package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
class LocationChangeAction(
  actor: Person,
  
  val newLocation: String,
  
  @Enumerated(EnumType.STRING)
  val means: TransportationMeans) : Action(actor)
