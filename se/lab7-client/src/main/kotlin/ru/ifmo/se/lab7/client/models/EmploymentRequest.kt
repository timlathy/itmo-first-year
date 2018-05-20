package ru.ifmo.se.lab7.client.models

import java.time.LocalDate

import javafx.beans.property.SimpleObjectProperty
import javafx.beans.property.SimpleStringProperty
import tornadofx.*
import java.time.format.DateTimeFormatter.ISO_DATE
import javax.json.JsonObject

typealias Location = Pair<Double, Double>

class EmploymentRequest(
  applicant: String = "",
  location: Location = Pair(0.0, 0.0),
  date: LocalDate = LocalDate.now(),
  details: String = "",
  status: Status = Status.PROCESSING
): JsonModel, Comparable<EmploymentRequest> {
  val applicantProperty = SimpleStringProperty(this, "applicant", applicant)
  var applicant by applicantProperty

  val locationProperty = SimpleObjectProperty<Location>(this, "location", location)
  var location by locationProperty

  val dateProperty = SimpleObjectProperty<LocalDate>(this, "date", date)
  var date by dateProperty

  val detailsProperty = SimpleStringProperty(this, "details", details)
  var details by detailsProperty

  val statusProperty = SimpleObjectProperty<Status>(this, "status", status)
  var status by statusProperty

  override fun updateModel(json: JsonObject) = with(json) {
    applicant = string("applicant")
    date = LocalDate.parse(string("date"), ISO_DATE)
    location = jsonArray("location").let { p -> println(p); Pair(0.0, 0.0) }
    details = string("details")
    status = Status.fromString(string("status"))
  }

  override fun toJSON(json: JsonBuilder) {
    with(json) {
      add("applicant", applicant)
      add("date", ISO_DATE.format(date))
      add("location", location.toList())
      add("details", details)
      add("status", status.toString())
    }
  }

  enum class Status(private val description: String) {
    INTERVIEW_SCHEDULED("Interview scheduled"),
    PROCESSING("Processing"),
    REJECTED("Rejected");

    companion object {
      fun fromString(description: String?) =
        Status.values().find { it.toString() == description } ?: PROCESSING
    }

    override fun toString() = description
  }

  override fun compareTo(other: EmploymentRequest): Int =
    Comparator.comparing<EmploymentRequest, Status> { it.status }
      .thenComparing<LocalDate> { it.date }
      .reversed().compare(this, other)
}

class EmploymentRequestModel(var employmentRequest: EmploymentRequest): ViewModel() {
  val applicant = bind { employmentRequest.applicantProperty }
  val location  = bind { employmentRequest.locationProperty }
  val date      = bind { employmentRequest.dateProperty }
  val details   = bind { employmentRequest.detailsProperty }
  val status    = bind { employmentRequest.statusProperty }
}
