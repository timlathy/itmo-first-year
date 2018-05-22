package ru.ifmo.se.lab7.client.models

import javafx.beans.property.*
import javafx.collections.FXCollections
import javafx.collections.ObservableMap
import ru.ifmo.se.lab7.client.models.EmploymentRequest.Status
import java.time.LocalDate
import tornadofx.*

class EmploymentRequestFilter(
  /* Abstraction leak: the control used for letter filtering (RangeSlider) only accepts Double values */
  minApplicantLetter: Double = 1.0,
  maxApplicantLetter: Double = 26.0,

  minLatitude: Double = 0.0,
  maxLatitude: Double = 90.0,
  minLongitude: Double = 0.0,
  maxLongitude: Double = 180.0,

  minDate: LocalDate? = null,
  maxDate: LocalDate? = null,

  includedStatuses: ObservableMap<Status, Boolean> =
    FXCollections.observableMap(mutableMapOf(
      Status.INTERVIEW_SCHEDULED to true,
      Status.PROCESSING          to true,
      Status.REJECTED            to true))
) {
  val minApplicantLetterProperty = SimpleDoubleProperty(minApplicantLetter)
  val minApplicantLetter by minApplicantLetterProperty

  val maxApplicantLetterProperty = SimpleDoubleProperty(maxApplicantLetter)
  val maxApplicantLetter by maxApplicantLetterProperty

  val minLatitudeProperty = SimpleDoubleProperty(minLatitude)
  val minLatitude by minLatitudeProperty

  val maxLatitudeProperty = SimpleDoubleProperty(maxLatitude)
  val maxLatitude by maxLatitudeProperty

  val minLongitudeProperty = SimpleDoubleProperty(minLongitude)
  val minLongitude by minLongitudeProperty

  val maxLongitudeProperty = SimpleDoubleProperty(maxLongitude)
  val maxLongitude by maxLongitudeProperty

  val minDateProperty = SimpleObjectProperty<LocalDate>(minDate)
  val minDate by minDateProperty

  val maxDateProperty = SimpleObjectProperty<LocalDate>(maxDate)
  val maxDate by maxDateProperty

  val includedStatusesProperty = SimpleMapProperty<Status, Boolean>(includedStatuses)
  val includedStatuses by includedStatusesProperty
}

class EmploymentRequestFilterModel(var filter: EmploymentRequestFilter): ViewModel() {
  val minApplicantLetter: SimpleDoubleProperty = bind { filter.minApplicantLetterProperty }
  val maxApplicantLetter: SimpleDoubleProperty = bind { filter.maxApplicantLetterProperty }

  val minLatitude: SimpleDoubleProperty = bind { filter.minLatitudeProperty }
  val maxLatitude: SimpleDoubleProperty = bind { filter.maxLatitudeProperty }

  val minLongitude: SimpleDoubleProperty = bind { filter.minLongitudeProperty }
  val maxLongitude: SimpleDoubleProperty = bind { filter.maxLongitudeProperty }

  val minDate: SimpleObjectProperty<LocalDate> = bind { filter.minDateProperty }
  val maxDate: SimpleObjectProperty<LocalDate> = bind { filter.maxDateProperty }

  val includedStatuses: SimpleMapProperty<Status, Boolean> = bind { filter.includedStatusesProperty }
}
