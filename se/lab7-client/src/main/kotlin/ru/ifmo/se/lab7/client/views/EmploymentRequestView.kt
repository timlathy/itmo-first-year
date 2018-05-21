package ru.ifmo.se.lab7.client.views

import javafx.geometry.Orientation
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequestModel
import ru.ifmo.se.lab7.client.models.LocationConverter
import tornadofx.*

class EmploymentRequestView: View() {
  val model = EmploymentRequestModel(EmploymentRequest())

  override val root = hbox {
    form {
      fieldset(labelPosition = Orientation.VERTICAL) {
        field("Applicant") {
          textfield(model.applicant)
        }
        field("Date") {
          datepicker(model.date)
        }
        field("Location") {
          textfield { bind(model.location, converter = LocationConverter()) }
        }
        field("Status") {
          combobox(model.status, EmploymentRequest.Status.values().toList())
        }
        field("Details") {
          textarea(model.details)
        }
        button("Save") {
          enableWhen(model.dirty)
        }
      }
    }
    vbox {
      styleClass.add("er-actions")
      label("Actions")
    }
  }
}
