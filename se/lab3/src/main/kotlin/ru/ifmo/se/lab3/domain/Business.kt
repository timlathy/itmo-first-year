package ru.ifmo.se.lab3.domain

import javax.persistence.*
import javax.validation.constraints.*
import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.core.JsonGenerator

@Entity
data class Business(
  val name: String,

  @OneToOne
  @JoinColumn(name="owner_id")
  @JsonSerialize(using = Person.ToNameStringSerializer::class)
  val owner: Person,

  @OneToMany(mappedBy = "employer")
  @JsonIgnore
  val employees: Set<Employment> = setOf(),

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

  /**
   * A serializer that can be used with @JsonSerialize to
   * convert a [Business] instance into a string with its [name] field.
   */
  class ToNameStringSerializer: JsonSerializer<Business>() {
    override fun serialize(business: Business, gen: JsonGenerator, _p: SerializerProvider) =
      gen.writeObject(business.name)
  }
}
