package ru.ifmo.se.lab3

import javax.persistence.*
import javax.validation.constraints.*
import java.time.LocalDateTime

import com.fasterxml.jackson.annotation.JsonIgnore

@Entity
data class BankTransaction(
  val amount: Int,

  @OneToOne
  @JoinTable(name = "bank_transaction_drawees")
  @JsonIgnore
  val draweeAccount: BankAccount,

  @OneToOne
  @JoinTable(name = "bank_transaction_drawers")
  @JsonIgnore
  val drawerAccount: BankAccount,

  val date: LocalDateTime,

  @Id @GeneratedValue
  val id: Long = -1) {

  fun drawee(): String = draweeAccount.transactionPartyTitle
  fun drawer(): String = drawerAccount.transactionPartyTitle
  
  /**
   * A DTO representing a new BankTransaction record.
   *
   * Drawee is assumed to be initiating the request,
   * drawer is instantiated by [BankTransactionService],
   * which performs further validation and converts the DTO to an entity bean.
   */
  data class Dto(
    @Min(1)
    val amount: Int,
    
    @NotNull
    val drawerId: Long)
}
