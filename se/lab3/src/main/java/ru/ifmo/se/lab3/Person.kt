package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
data class Person(
  val name: String,

  @Id @GeneratedValue
  val id: Long = -1,
  
  @OneToOne
  @JoinColumn(name="bank_account_id")
  var bankAccount: BankAccount) : TransactionParty {

  override fun getTransactionParty() = this
  override fun getTransactionPartyLabel() = "Individual $name"
  override fun getTransactionAccount() = bankAccount
}
