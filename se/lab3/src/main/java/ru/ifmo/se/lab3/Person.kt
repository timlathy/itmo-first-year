package ru.ifmo.se.lab3

import javax.persistence.*
import com.fasterxml.jackson.annotation.JsonIgnore

@Entity
data class Person(
  val name: String,

  @Id @GeneratedValue
  val id: Long = -1,

  @OneToOne
  @JoinColumn(name="bank_account_id")
  var bankAccount: BankAccount) : TransactionParty {

  @JsonIgnore
  override fun getTransactionParty() = this
  @JsonIgnore
  override fun getTransactionPartyLabel() = "Individual $name"
  @JsonIgnore
  override fun getTransactionAccount() = bankAccount
}
