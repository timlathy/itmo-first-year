package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
data class BankAccount(
  var balance: Int,

  val name: String,

  @OneToOne
  @JoinColumn(name="owner_id")
  val owner: Person,

  @Id @GeneratedValue
  val id: Long = -1)
