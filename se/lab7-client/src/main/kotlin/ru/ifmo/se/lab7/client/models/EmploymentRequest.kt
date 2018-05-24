package ru.ifmo.se.lab7.client.models

import java.time.LocalDate

import javafx.beans.property.SimpleObjectProperty
import javafx.beans.property.SimpleStringProperty
import javafx.util.StringConverter
import tornadofx.*
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.format.DateTimeFormatter.ISO_DATE_TIME
import javax.json.*

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

  val LOCATION_API_COMPAT = "interviewLocation"

  override fun updateModel(json: JsonObject) = with(json) {
    applicant = string("applicant")
    date = LocalDate.parse(string("date"), ISO_DATE_TIME)
    location = jsonArray(LOCATION_API_COMPAT).let { p: JsonArray? ->
      p?.takeIf { it.size == 2 }?.let {
        val latitude = (it[0] as? JsonNumber)?.doubleValue()
        val longitude = (it[1] as? JsonNumber)?.doubleValue()
        if (latitude != null && longitude != null) Pair(latitude, longitude)
        else Pair(0.0, 0.0) } ?: Pair(0.0, 0.0)
    }
    details = string("details")
    status = Status.fromString(string("status"))
  }

  override fun toJSON(json: JsonBuilder) {
    with(json) {
      add("applicant", applicant)
      add("date", ISO_DATE_TIME.format(LocalDateTime.of(date, LocalTime.MIDNIGHT)))
      add(LOCATION_API_COMPAT, location.let {
        Json.createArrayBuilder(mutableListOf(it.first, it.second))
      })
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

class LocationConverter: StringConverter<Location>() {
  override fun toString(loc: Location): String =
    "(${loc.first}, ${loc.second})"

  override fun fromString(string: String): Location =
    string.trimStart('(', ' ').trimEnd(')', ' ').split(",").let { pair ->
      Location(pair.first().toDouble(), pair.last().toDouble())
    }
}
