package ru.ifmo.se.lab7.client.models

import javafx.beans.property.SimpleDoubleProperty
import javafx.beans.property.SimpleIntegerProperty
import java.time.LocalDate

import javafx.beans.property.SimpleObjectProperty
import javafx.beans.property.SimpleStringProperty
import tornadofx.*
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.format.DateTimeFormatter.ISO_DATE_TIME
import javax.json.*

class EmploymentRequest(
  id: Int = 0,
  applicant: String = "",
  locLatitude: Double = 0.0,
  locLongitude: Double = 0.0,
  date: LocalDate = LocalDate.now(),
  details: String = "",
  status: Status = Status.PROCESSING,
  colorCode: ColorCode = ColorCode.ORANGE
): JsonModel, Comparable<EmploymentRequest> {
  val idProperty = SimpleIntegerProperty(this, "id", id)
  var id by idProperty

  val applicantProperty = SimpleStringProperty(this, "applicant", applicant)
  var applicant by applicantProperty

  val locLatitudeProperty = SimpleDoubleProperty(this, "locLatitude", locLatitude)
  var locLatitude by locLatitudeProperty

  val locLongitudeProperty = SimpleDoubleProperty(this, "locLongitude", locLongitude)
  var locLongitude by locLongitudeProperty

  val dateProperty = SimpleObjectProperty<LocalDate>(this, "date", date)
  var date by dateProperty

  val detailsProperty = SimpleStringProperty(this, "details", details)
  var details by detailsProperty

  val statusProperty = SimpleObjectProperty<Status>(this, "status", status)
  var status by statusProperty

  val colorCodeProperty = SimpleObjectProperty<ColorCode>(this, "colorCode", colorCode)
  var colorCode by colorCodeProperty

  override fun updateModel(json: JsonObject) = with(json) {
    id = int("id")!!
    applicant = string("applicant")
    date = LocalDate.parse(string("date"), ISO_DATE_TIME)
    locLatitude = double("locLatitude") ?: 0.0
    locLongitude = double("locLongitude") ?: 0.0
    details = string("details")
    status = string("status")?.let(Status::valueOf) ?: Status.PROCESSING
    colorCode = string("colorCode")?.let(ColorCode::valueOf) ?: ColorCode.ORANGE
  }

  override fun toJSON(json: JsonBuilder) {
    with(json) {
      add("id", id)
      add("applicant", applicant)
      add("date", ISO_DATE_TIME.format(LocalDateTime.of(date, LocalTime.MIDNIGHT)))
      add("locLatitude", locLatitude)
      add("locLongitude", locLongitude)
      add("details", details)
      add("status", status.toString())
      add("colorCode", colorCode.toString())
    }
  }

  enum class Status { INTERVIEW_SCHEDULED, PROCESSING, REJECTED }

  enum class ColorCode { ORANGE, BLUE, GREEN }

  override fun compareTo(other: EmploymentRequest): Int =
    Comparator.comparing<EmploymentRequest, Status> { it.status }
      .thenComparing<LocalDate> { it.date }
      .reversed().compare(this, other)
}

class EmploymentRequestModel(var employmentRequest: EmploymentRequest): ViewModel() {
  val id           = bind { employmentRequest.idProperty }
  val applicant    = bind { employmentRequest.applicantProperty }
  val locLatitude  = bind { employmentRequest.locLatitudeProperty }
  val locLongitude = bind { employmentRequest.locLongitudeProperty }
  val date         = bind { employmentRequest.dateProperty }
  val details      = bind { employmentRequest.detailsProperty }
  val status       = bind { employmentRequest.statusProperty }
  val colorCode    = bind { employmentRequest.colorCodeProperty }
}
