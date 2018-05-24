package ru.ifmo.se.lab7.client.views

import javafx.beans.property.SimpleBooleanProperty
import javafx.geometry.Orientation
import javafx.geometry.Pos
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequestModel
import ru.ifmo.se.lab7.client.models.LocationConverter
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController.Actions
import tornadofx.*

class EmploymentRequestView: View() {
  val model = EmploymentRequestModel(EmploymentRequest())

  var isNewModel: SimpleBooleanProperty = SimpleBooleanProperty(false)

  class ObjectActionRequest(val action: Actions, val element: EmploymentRequest, val auxElement: EmploymentRequest? = null): FXEvent()

  private fun setViewTitle(applicant: String?) {
    title = (if (applicant == null || applicant.isBlank()) "New employment request"
             else "${applicant}'s employment request").toUpperCase()
  }

  override val root = hbox {
    alignment = Pos.TOP_CENTER
    setViewTitle(model.applicant.getValue())
    model.applicant.onChange(::setViewTitle)

    form {
      fieldset(labelPosition = Orientation.VERTICAL) {
        field("Applicant") {
          textfield(model.applicant).required(ValidationTrigger.None)
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
        field("Color code") {
          combobox(model.colorCode, EmploymentRequest.ColorCode.values().toList())
        }
        field("Details") {
          textarea(model.details)
        }
        button("Save changes") {
          enableWhen(model.dirty)
          visibleWhen(isNewModel.not())
          setOnAction {
            val oldRequest = model.employmentRequest
            model.validate()
            if (model.commit()) fire(ObjectActionRequest(Actions.CHANGE_EXISTING, model.employmentRequest, oldRequest))
          }
        }
        button("Remove element") {
          visibleWhen(isNewModel.not())
          setOnAction {
            if (model.commit()) fire(ObjectActionRequest(Actions.REMOVE, model.employmentRequest))
          }
        }
      }
    }
    vbox {
      spacing = 8.0
      styleClass.add("er-actions")
      label("Actions").apply {
        styleClass.add("er-actions__header")
      }
      label("Use this employment request\nas a template to:").apply {
        styleClass.add("er-actions__info")
        isWrapText = true
      }
      button("Remove all with higher priority").apply {
        styleClass.add("er-actions__button")
        setOnAction {
          fire(ObjectActionRequest(Actions.REMOVE_ALL_HIGHER_PRIORITY, model.employmentRequest))
        }
      }
      button("Remove all with lower priority").apply {
        styleClass.add("er-actions__button")
        setOnAction {
          fire(ObjectActionRequest(Actions.REMOVE_ALL_LOWER_PRIORITY, model.employmentRequest))
        }
      }
      button("Remove all equivalent").apply {
        styleClass.add("er-actions__button")
        setOnAction {
          fire(ObjectActionRequest(Actions.REMOVE_ALL, model.employmentRequest))
        }
      }
      label("Add this employment request\nto the queue:").apply {
        styleClass.add("er-actions__info")
        isWrapText = true
      }
      button("Only if it has the highest priority").apply {
        styleClass.add("er-actions__button")
        setOnAction {
          if (model.commit()) fire(ObjectActionRequest(Actions.ADD_IF_HIGHEST_PRIORITY, model.employmentRequest))
        }
      }
      button("Only if it has the lowest priority").apply {
        styleClass.add("er-actions__button")
        setOnAction {
          if (model.commit()) fire(ObjectActionRequest(Actions.ADD_IF_LOWEST_PRIORITY, model.employmentRequest))
        }
      }
      button("Add unconditionally").apply {
        styleClass.add("er-actions__button")
        setOnAction {
          if (model.commit()) fire(ObjectActionRequest(Actions.ADD, model.employmentRequest))
        }
      }
    }
  }
}
