package ru.ifmo.se.lab3.domain

import javax.persistence.*
import javax.validation.constraints.*
import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.databind.annotation.JsonSerialize

@Entity
@Table(uniqueConstraints=[UniqueConstraint(columnNames=["employee_id", "employer_id"])])
data class Employment(
  @OneToOne
  @JoinColumn(name="employee_id")
  @JsonSerialize(using = Person.ToNameStringSerializer::class)
  val employee: Person,

  @OneToOne
  @JoinColumn(name="employer_id")
  @JsonSerialize(using = Business.ToNameStringSerializer::class)
  val employer: Business,

  val title: String,
  
  @Id @GeneratedValue
  val id: Long = -1) {

  /**
   * A DTO representing a new Employment record.
   *
   * Employee-employer relationships are considered unique,
   * that is, there can be no more than a single Employment
   * entry for a particular pair of employer and employee.
   * 
   * Constraint violations are to be handled by
   * [EmploymentService], which is also responsible for converting
   * the DTO to an entity bean.
   */
  data class Dto(
    @NotBlank
    val employeeName: String,

    @NotBlank
    val employerName: String,

    @NotBlank
    val title: String)
}

