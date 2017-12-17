package ru.ifmo.se.lab3.domain

import javax.persistence.*
import javax.validation.constraints.*

@Entity
data class BankAccount(
  var balance: Int,

  @Column(unique = true)
  val name: String,

  @OneToOne
  @JoinColumn(name="owner_id")
  val owner: Person,

  @Id @GeneratedValue
  val id: Long = -1) {
  
  /**
   * A DTO representing a new BankAccount record.
   *
   * Owner is instantiated from name by [BankAccountService],
   * which also converts the DTO to an entity bean.
   */
  data class Dto(
    @NotNull
    val balance: Int,

    @NotBlank
    val name: String,
    
    @NotBlank
    val ownerName: String)
}
