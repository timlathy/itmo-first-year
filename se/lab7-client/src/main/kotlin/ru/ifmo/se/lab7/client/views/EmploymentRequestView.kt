package ru.ifmo.se.lab7.client.views

import javafx.geometry.Orientation
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequestModel
import tornadofx.*

class EmploymentRequestView: View() {
  val model = EmploymentRequestModel(EmploymentRequest())

  override val root = vbox {
    form {
      fieldset(labelPosition = Orientation.VERTICAL) {
        field("Applicant") {
          textfield(model.applicant)
        }
        field("Date") {
          datepicker(model.date)
        }
      }
    }
  }
}
