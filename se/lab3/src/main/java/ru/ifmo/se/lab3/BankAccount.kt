package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
data class BankAccount(
  var balance: Int,

  @Id @GeneratedValue
  val id: Long = -1)
