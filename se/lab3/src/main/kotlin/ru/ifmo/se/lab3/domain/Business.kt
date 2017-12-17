package ru.ifmo.se.lab3.domain

import javax.persistence.*
import javax.validation.constraints.*
import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.databind.annotation.JsonSerialize

@Entity
data class Business(
  val name: String,

  @OneToOne
  @JoinColumn(name="owner_id")
  @JsonSerialize(using = Person.ToNameStringSerializer::class)
  val owner: Person,

  @ManyToMany
  @JoinTable(name = "business_employees")
  @JsonIgnore
  val employees: Set<Person> = setOf(),

  @Id @GeneratedValue
  val id: Long = -1) {

  /**
   * A DTO representing a new Business record.
   *
   * Owner is instantiated by [BusinessService], which
   * performs further validation and converts the DTO to an entity bean.
   */
  data class Dto(
    @NotBlank
    val name: String,
    
    @NotBlank
    val ownerName: String)
}
