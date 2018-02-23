package ru.ifmo.se.lab5

import java.time.LocalDateTime

import com.fasterxml.jackson.annotation.*
import com.fasterxml.jackson.databind.*

data class EmploymentRequest(
  val applicant: String,

  val date: LocalDateTime,

  var status: EmploymentRequest.Status = Status.PROCESSING,

  var details: String = "") {

  enum class Status(private val description: String) {
    PROCESSING("Processing"),
    INTERVIEW_SCHEDULED("Interview scheduled"),
    REJECTED("Rejected");

    @JsonValue
    override fun toString() = description
  }
}
