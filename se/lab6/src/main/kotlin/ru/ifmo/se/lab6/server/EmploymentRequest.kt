package ru.ifmo.se.lab6.server

import java.time.LocalDateTime

import com.fasterxml.jackson.annotation.*
import javax.validation.constraints.NotBlank
import javax.validation.constraints.PastOrPresent

data class EmploymentRequest(
  @field:NotBlank(message = "applicant name cannot be blank")
  val applicant: String = "",

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss")
  @field:PastOrPresent(message = "request date cannot refer to the future")
  val date: LocalDateTime = LocalDateTime.now(),

  @JsonFormat(shape = JsonFormat.Shape.ARRAY)
  val interviewLocation: Pair<Double, Double>? = null,

  val details: String = "",

  val status: EmploymentRequest.Status = Status.PROCESSING): Comparable<EmploymentRequest> {

  enum class Status(private val description: String) {
    INTERVIEW_SCHEDULED("Interview scheduled"),
    PROCESSING("Processing"),
    REJECTED("Rejected");

    @JsonValue
    override fun toString() = description
  }

  override fun compareTo(other: EmploymentRequest): Int =
    Comparator.comparing<EmploymentRequest, Status> { it.status }
      .thenComparing<LocalDateTime> { it.date }
      .reversed().compare(this, other)
}
