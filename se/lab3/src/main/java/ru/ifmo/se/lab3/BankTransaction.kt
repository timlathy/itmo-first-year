package ru.ifmo.se.lab3

import javax.persistence.*
import java.time.LocalDateTime

@Entity
data class BankTransaction(
  val amount: Int,

  @OneToOne
  @JoinTable(name = "bank_transaction_drawees")
  val drawee: Person,
  
  val draweeLabel: String,

  @OneToOne
  @JoinTable(name = "bank_transaction_drawers")
  val drawer: Person,

  val drawerLabel: String,

  val date: LocalDateTime,

  @Id @GeneratedValue
  val id: Long = -1)
