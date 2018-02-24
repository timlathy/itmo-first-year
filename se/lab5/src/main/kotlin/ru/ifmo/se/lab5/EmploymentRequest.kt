package ru.ifmo.se.lab5

import java.time.LocalDateTime

import com.fasterxml.jackson.annotation.*
import javax.validation.constraints.NotBlank
import javax.validation.constraints.PastOrPresent

data class EmploymentRequest(
  @field:NotBlank(message = "applicant name cannot be blank")
  val applicant: String = "",

  @field:PastOrPresent(message = "request date cannot refer to the future")
  val date: LocalDateTime = LocalDateTime.now(),

  val details: String = "",

  val status: EmploymentRequest.Status = Status.PROCESSING) {

  enum class Status(private val description: String) {
    PROCESSING("Processing"),
    INTERVIEW_SCHEDULED("Interview scheduled"),
    REJECTED("Rejected");

    @JsonValue
    override fun toString() = description
  }
}
