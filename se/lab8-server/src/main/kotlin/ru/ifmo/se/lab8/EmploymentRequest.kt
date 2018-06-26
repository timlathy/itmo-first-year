package ru.ifmo.se.lab8

import java.time.ZonedDateTime

import com.fasterxml.jackson.annotation.*
import org.pearl.Changeset
import org.pearl.Id
import org.pearl.Model
import org.pearl.query.SelectQuery.SortDirection.*
import org.pearl.query.SelectQuery
import org.pearl.query.exists
import org.pearl.query.from
import org.pearl.query.not

typealias EmReq = EmploymentRequest

@JsonIgnoreProperties(value = ["tableName", "schema"])
data class EmploymentRequest(
  @Id
  val id: Int = 0,

  val applicant: String = "",

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ssxxxxx")
  val date: ZonedDateTime = ZonedDateTime.now(),

  val locLatitude: Double = 0.0,

  val locLongitude: Double = 0.0,

  val details: String = "",

  val status: EmploymentRequest.Status = Status.PROCESSING,

  val colorCode: EmploymentRequest.ColorCode = ColorCode.ORANGE)
  : Comparable<EmReq>, Model() {

  companion object {
    val ALLOWED_PARAMS = listOf("applicant", "date", "locLatitude", "locLongitude", "details", "status", "colorCode")

    fun validate(changeset: Changeset<EmReq>) = changeset
      .validate<String>("applicant", { !it.isNullOrBlank() },
        "Applicant name must be present and cannot be blank.")
      .validate<Double>("locLatitude", { it != null && it in -90.0..90.0 },
        "Applicant location must be present, and its latitude must range from -90 to 90 deg")
      .validate<Double>("locLongitude", { it != null && it in -180.0..180.0 },
        "Applicant location must be present, and its longitude must range from -180 to 180 deg")

    fun isMin(changeset: Changeset<EmReq>) =
      not(exists(from<EmReq>().where { it lesserThan changeset }))

    fun isMax(changeset: Changeset<EmReq>) =
      not(exists(from<EmReq>().where { it greaterThan changeset }))

    fun ascOrder(query: SelectQuery<EmReq>) = query.orderBy("date", DESC)

    fun descOrder(query: SelectQuery<EmReq>) = query.orderBy("date", ASC)
  }

  enum class Status { INTERVIEW_SCHEDULED, PROCESSING, REJECTED }

  enum class ColorCode { ORANGE, BLUE, GREEN }

  override fun compareTo(other: EmReq): Int =
    Comparator.comparing<EmReq, ZonedDateTime> { it.date }.reversed().compare(this, other)

  infix fun greaterThan(changeset: Changeset<EmReq>) =
    this["date"] lte changeset.changes["date"] as ZonedDateTime

  infix fun lesserThan(changeset: Changeset<EmReq>) =
    this["date"] gte changeset.changes["date"] as ZonedDateTime

  @JsonIgnoreProperties
  override val tableName = super.tableName
}
