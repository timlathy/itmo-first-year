package ru.ifmo.se.lab3

import javax.persistence.*
import com.fasterxml.jackson.annotation.JsonIgnore

@Entity
data class Business(
  val name: String,

  @OneToOne
  @JoinColumn(name="owner_id")
  var owner: Person,
  
  @Id @GeneratedValue
  val id: Long = -1) : TransactionParty {

  @JsonIgnore
  override fun getTransactionParty() = owner
  @JsonIgnore
  override fun getTransactionPartyLabel() = "Business $name"
  @JsonIgnore
  override fun getTransactionAccount() = owner.bankAccount
}
