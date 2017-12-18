package ru.ifmo.se.lab3.domain

import java.time.LocalDateTime
import javax.persistence.*
import javax.validation.constraints.*
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.databind.annotation.JsonSerialize

@Entity
data class EmploymentRequest(
  @OneToOne
  @JoinColumn(name="applicant_id")
  @JsonSerialize(using = Person.ToNameStringSerializer::class)
  val applicant: Person,

  val date: LocalDateTime,

  @Enumerated(EnumType.STRING)
  var status: EmploymentRequest.Status = Status.PROCESSING,

  var details: String = "",

  @Id @GeneratedValue
  val id: Long = -1) {

  enum class Status(private val description: String) {
    PROCESSING("Processing"),
    INTERVIEW_SCHEDULED("Interview scheduled"),
    REJECTED("Rejected");

    @JsonValue
    override fun toString() = description
  }

  /**
   * A DTO representing updates to an existent EmploymentRequest record.
   *
   * Initial applications are instaniated entirely through
   * [EmploymentRequestService].
   */
  data class UpdateDto(
    @NotNull
    val status: EmploymentRequest.Status,
    
    @NotBlank
    val details: String)
}
