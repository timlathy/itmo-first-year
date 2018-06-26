package ru.ifmo.se.lab7.client.views

import javafx.collections.ListChangeListener
import javafx.geometry.Orientation
import javafx.geometry.Pos
import javafx.util.StringConverter
import ru.ifmo.se.lab7.client.LocaleControl
import ru.ifmo.se.lab7.client.i18n
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import ru.ifmo.se.lab7.client.models.EmploymentRequestFilter
import ru.ifmo.se.lab7.client.models.EmploymentRequestFilterModel
import ru.ifmo.se.lab7.client.models.Validators.Companion.textContainingDoubleBetween
import ru.ifmo.se.lab7.client.models.Converters.Companion.SafeNumberStringConverter

import tornadofx.*
import tornadofx.controlsfx.*
import java.util.function.Predicate

class FilterView: View() {
  private val model = EmploymentRequestFilterModel(EmploymentRequestFilter())

  init {
    titleProperty.i18n("filters.title")
  }

  fun hasFiltersApplied() = model.isDirty && model.isValid

  fun compileFilterPredicate(): ((EmploymentRequest) -> Boolean) {
    var pred = Predicate<EmploymentRequest> { true }

    if (model.minApplicantLetter.isDirty) pred = pred.and {
      it.applicant[0].toInt() >= (model.minApplicantLetter.get().toInt() + 65)
    }
    if (model.maxApplicantLetter.isDirty) pred = pred.and {
      it.applicant[0].toInt() <= (model.maxApplicantLetter.get().toInt() + 65)
    }

    if (model.minLatitude.isDirty) pred = pred.and {
      it.locLatitude >= model.minLatitude.get()
    }
    if (model.maxLatitude.isDirty) pred = pred.and {
      it.locLatitude <= model.maxLatitude.get()
    }

    if (model.minLongitude.isDirty) pred = pred.and {
      it.locLongitude >= model.minLongitude.get()
    }
    if (model.maxLongitude.isDirty) pred = pred.and {
      it.locLongitude <= model.maxLongitude.get()
    }

    if (model.minDate.isDirty) pred = pred.and {
      it.date >= model.minDate.get()
    }
    if (model.maxDate.isDirty) pred = pred.and {
      it.date <= model.maxDate.get()
    }

    if (model.includedStatuses.values.any { !it }) pred = pred.and {
      it.status in model.includedStatuses.entries.filter { it.value }.map { it.key }
    }

    if (model.includedColorCodes.values.any { !it }) pred = pred.and {
      it.colorCode in model.includedColorCodes.entries.filter { it.value }.map { it.key }
    }

    return (pred::test)
  }

  override val root = form {
    fieldset(labelPosition = Orientation.VERTICAL) {
      field {
        labelProperty.i18n("filters.name")
        rangeslider(model.minApplicantLetter, model.maxApplicantLetter, 1.0, 26.0) {
          blockIncrement = 1.0
          majorTickUnit = 1.0
          minorTickCount = 0

          isShowTickLabels = true
          isShowTickMarks = true
          isSnapToTicks = true

          labelFormatter = object : StringConverter<Number>() {
            override fun toString(tick: Number?): String = tick?.let { (it.toInt() + 64).toChar().toString() } ?: ""
            override fun fromString(string: String?): Number = string?.toInt() ?: 1.0
          }
        }
      }
      field(orientation = Orientation.VERTICAL) {
        labelProperty.i18n("filters.location")
        hbox {
          alignment = Pos.BASELINE_LEFT
          spacing = 12.0

          textfield(model.minLatitude, SafeNumberStringConverter()) {
            required()
            textContainingDoubleBetween(-90.0, 90.0)
          }
          label {
            textProperty().i18n("filters.upto")
          }
          textfield(model.maxLatitude, SafeNumberStringConverter()) {
            required()
            textContainingDoubleBetween(-90.0, 90.0)
          }
          label {
            textProperty().i18n("filters.latitude")
          }
        }
        hbox {
          alignment = Pos.BASELINE_LEFT
          spacing = 12.0

          textfield(model.minLongitude, SafeNumberStringConverter()) {
            required()
            textContainingDoubleBetween(-180.0, 180.0)
          }
          label {
            textProperty().i18n("filters.upto")
          }
          textfield(model.maxLongitude, SafeNumberStringConverter()) {
            required()
            textContainingDoubleBetween(-180.0, 180.0)
          }
          label {
            textProperty().i18n("filters.longitude")
          }
        }
      }
      field {
        labelProperty.i18n("filters.date")
        datepicker(model.minDate); label { textProperty().i18n("filters.upto") }; datepicker(model.maxDate)
      }
      field {
        labelProperty.i18n("filters.status")
        val list = observableList(*model.includedStatuses.keys.toTypedArray())
        val listView = checklistview(list).apply {
          /* https://stackoverflow.com/a/17456527/1726690 */
          prefHeight = list.size * 26 + 2.0

          model.includedStatuses.forEach { k, v ->
            if (v) checkModel.check(k)
            else checkModel.clearCheck(k)
          }

          checkModel.checkedItems.addListener { c: ListChangeListener.Change<out EmploymentRequest.Status> ->
            c.next()
            c.addedSubList.forEach {
              model.includedStatuses[it] = true
            }
            c.removed.forEach {
              model.includedStatuses[it] = false
              model.includedStatuses.markDirty()
            }
          }
        }

        LocaleControl.resourcesProperty().onChange {
          list.removeAll()
          list.addAll(*model.includedStatuses.keys.toTypedArray())

          model.includedStatuses.forEach { k, v ->
            if (v) listView.checkModel.check(k)
            else listView.checkModel.clearCheck(k)
          }
        }
      }
      field {
        labelProperty.i18n("filters.color_codes")
        val list = observableList(*model.includedColorCodes.keys.toTypedArray())
        val listView = checklistview(list).apply {
          /* https://stackoverflow.com/a/17456527/1726690 */
          prefHeight = list.size * 26 + 2.0

          model.includedColorCodes.forEach { k, v ->
            if (v) checkModel.check(k)
            else checkModel.clearCheck(k)
          }

          checkModel.checkedItems.addListener { c: ListChangeListener.Change<out EmploymentRequest.ColorCode> ->
            c.next()
            c.addedSubList.forEach {
              model.includedColorCodes[it] = true
            }
            c.removed.forEach {
              model.includedColorCodes[it] = false
              model.includedColorCodes.markDirty()
            }
          }
        }

        LocaleControl.resourcesProperty().onChange {
          list.removeAll()
          list.addAll(*model.includedColorCodes.keys.toTypedArray())

          model.includedColorCodes.forEach { k, v ->
            if (v) listView.checkModel.check(k)
            else listView.checkModel.clearCheck(k)
          }
        }
      }
    }
  }
}
