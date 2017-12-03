package ru.ifmo.se.lab3

import com.fasterxml.jackson.annotation.JsonValue

enum class TransportationMeans(private val description: String) {
  BY_FOOT("By foot"),
  PUBLIC_TRANSIT("Public transit"),
  TAXI("Registered autonomous taxi"),
  PRIVATE_VEHICLE("Privately owned vehicle"),
  OTHER("Other");

  @JsonValue
  override fun toString() = description
}
