package ru.ifmo.se.lab7.client.views

import javafx.beans.property.SimpleBooleanProperty
import javafx.geometry.Orientation
import javafx.geometry.Pos
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequestModel
import ru.ifmo.se.lab7.client.controllers.EmploymentRequestController.Actions
import ru.ifmo.se.lab7.client.i18n
import ru.ifmo.se.lab7.client.models.Validators.Companion.textContainingDoubleBetween
import tornadofx.*

class EmploymentRequestView: View() {
  companion object {
    val DOUBLE_REGEX = """\d+([,.]\d+)?""".toRegex()
  }

  val model = EmploymentRequestModel(EmploymentRequest())

  var isNewModel: SimpleBooleanProperty = SimpleBooleanProperty(false)

  class ObjectActionRequest(val action: Actions, val element: EmploymentRequest, val auxElement: EmploymentRequest? = null): FXEvent()

  private fun setViewTitle(applicant: String?) {
    title = (if (applicant == null || applicant.isBlank()) messages["erview.new"]
             else messages["erview.editing"].replace("{}", applicant)).toUpperCase()
  }

  override val root = hbox {
    alignment = Pos.TOP_CENTER
    setViewTitle(model.applicant.getValue())
    model.applicant.onChange(::setViewTitle)

    form {
      fieldset(labelPosition = Orientation.VERTICAL) {
        field {
          labelProperty.i18n("erview.applicant")
          textfield(model.applicant).required(ValidationTrigger.None)
        }
        field {
          labelProperty.i18n("erview.date")
          datepicker(model.date)
        }
        field {
          labelProperty.i18n("erview.location")
          textfield {
            bind(model.locLatitude)
            textContainingDoubleBetween(-90.0, 90.0)
          }
          textfield {
            bind(model.locLongitude)
            textContainingDoubleBetween(-180.0, 180.0)
          }
        }
        field {
          labelProperty.i18n("erview.status")
          combobox(model.status, EmploymentRequest.Status.values().toList())
        }
        field {
          labelProperty.i18n("erview.color_code")
          combobox(model.colorCode, EmploymentRequest.ColorCode.values().toList())
        }
        field {
          labelProperty.i18n("erview.details")
          textarea(model.details)
        }
        hbox {
          spacing = 8.0

          button {
            textProperty().i18n("erview.save")
            styleClass.add("form__button")
            enableWhen(model.dirty)
            visibleWhen(isNewModel.not())
            setOnAction {
              val oldRequest = model.employmentRequest
              model.validate()
              if (model.commit()) fire(ObjectActionRequest(Actions.EDIT, model.employmentRequest, oldRequest))
            }
          }

          button {
            textProperty().i18n("erview.remove")
            styleClass.add("form__button")
            visibleWhen(isNewModel.not())
            setOnAction {
              if (model.commit()) fire(ObjectActionRequest(Actions.REMOVE, model.employmentRequest))
            }
          }
        }
      }
    }
    vbox {
      spacing = 8.0
      styleClass.add("er-actions")
      label {
        textProperty().i18n("erview.actions")
        styleClass.add("er-actions__header")
      }
      label {
        textProperty().i18n("erview.actions_info")
        styleClass.add("er-actions__info")
        isWrapText = true
      }
      button {
        textProperty().i18n("erview.remove_higher")
        styleClass.add("er-actions__button")
        setOnAction {
          fire(ObjectActionRequest(Actions.REMOVE_ALL_HIGHER_PRIORITY, model.employmentRequest))
        }
      }
      button {
        textProperty().i18n("erview.remove_lower")
        styleClass.add("er-actions__button")
        setOnAction {
          fire(ObjectActionRequest(Actions.REMOVE_ALL_LOWER_PRIORITY, model.employmentRequest))
        }
      }
      button {
        textProperty().i18n("erview.remove_all")
        styleClass.add("er-actions__button")
        setOnAction {
          fire(ObjectActionRequest(Actions.REMOVE_ALL, model.employmentRequest))
        }
      }
      label {
        textProperty().i18n("erview.add")
        styleClass.add("er-actions__info")
        isWrapText = true
      }
      button {
        textProperty().i18n("erview.add_if_max")
        styleClass.add("er-actions__button")
        setOnAction {
          if (model.commit()) fire(ObjectActionRequest(Actions.ADD_IF_HIGHEST_PRIORITY, model.employmentRequest))
        }
      }
      button {
        textProperty().i18n("erview.add_if_min")
        styleClass.add("er-actions__button")
        setOnAction {
          if (model.commit()) fire(ObjectActionRequest(Actions.ADD_IF_LOWEST_PRIORITY, model.employmentRequest))
        }
      }
      button {
        textProperty().i18n("erview.add_uncond")
        styleClass.add("er-actions__button")
        setOnAction {
          if (model.commit()) fire(ObjectActionRequest(Actions.ADD, model.employmentRequest))
        }
      }
    }
  }
}
