package ru.ifmo.se.lab3

import javax.persistence.*
import com.fasterxml.jackson.annotation.JsonIgnore

@Entity
data class Person(
  val name: String,

  @Id @GeneratedValue
  val id: Long = -1)
