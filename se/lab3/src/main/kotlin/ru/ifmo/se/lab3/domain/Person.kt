package ru.ifmo.se.lab3.domain

import javax.persistence.*
import javax.validation.constraints.*
import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.core.JsonGenerator

@Entity
data class Person(
  @Column(unique = true)
  val name: String,
  
  @Id @GeneratedValue
  val id: Long = -1) {

  @JsonIgnore
  @OneToOne(mappedBy = "owner")
  var account: BankAccount? = null

  @JsonProperty
  fun getSocialClass(): PersonSocialClass? =
    account?.balance?.let { PersonSocialClass.fromAccountBalance(it) }

  /**
   * A DTO representing a new Person record.
   *
   * Name uniqueness constraint violations are expected
   * to be handled by [PersonService], which is also
   * responsible for converting the DTO to an entity bean.
   */
  data class Dto(
    @NotBlank
    val name: String)

  /**
   * A serializer that can be used with @JsonSerialize to
   * convert a [Person] instance into a string with its [name] field.
   */
  class ToNameStringSerializer: JsonSerializer<Person>() {
    override fun serialize(person: Person, gen: JsonGenerator, _p: SerializerProvider) =
      gen.writeObject(person.name)
  }
}

