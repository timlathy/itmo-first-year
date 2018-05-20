package ru.ifmo.se.lab7.server.ui

import com.github.lgooddatepicker.components.DatePickerSettings
import com.github.lgooddatepicker.components.DateTimePicker
import com.github.lgooddatepicker.components.TimePickerSettings
import ru.ifmo.se.lab7.server.EmploymentRequest
import ru.ifmo.se.lab7.server.ObjectValidator
import java.awt.event.ActionEvent
import java.time.LocalDateTime
import java.util.*
import javax.swing.*
import javax.swing.event.EventListenerList

const val VALIDATION_FAILED = "The request could not be saved due to the following problems:"

class EmploymentRequestEditorComponent: JComponent() {
  private val validator = ObjectValidator<EmploymentRequest>()

  //<editor-fold defaultstate="collapsed" desc="Events">
  class EditingFinishEvent(source: Any, val newObject: EmploymentRequest): EventObject(source)

  interface EditingFinishEventListener: EventListener {
    fun onEditingFinish(e: EditingFinishEvent)
  }

  val eventListeners = EventListenerList()

  fun addEditingFinishListener(listener: EditingFinishEventListener) =
    eventListeners.add(EditingFinishEventListener::class.java, listener)

  private fun triggerEditingFinishEvent(e: EditingFinishEvent) =
    eventListeners.getListeners(EditingFinishEventListener::class.java).forEach { it.onEditingFinish(e) }
  //</editor-fold>

  private val labelApplicant = JLabel("Applicant")
  private val fieldApplicant = JTextField(20)
  private val labelStatus = JLabel("Status")
  private val dropdownStatus = JComboBox(EmploymentRequest.Status.values())
  private val labelDate = JLabel("Date")
  private val pickerDate = run {
    val dateSettings = DatePickerSettings().apply { allowEmptyDates = false }
    val timeSettings = TimePickerSettings().apply { allowEmptyTimes = false }
    DateTimePicker(dateSettings, timeSettings).apply { dateTimeStrict = LocalDateTime.now() }
  }
  private val buttonSave = JButton("Save").apply { addActionListener(::finishEditing) }
  private val labelViolations = JLabel()

  init {
    layout = GroupLayout(this).apply {
      autoCreateGaps = true
      autoCreateContainerGaps = true

      setHorizontalGroup(
        createParallelGroup(GroupLayout.Alignment.LEADING)
          .addGroup(createSequentialGroup()
            .addGroup(createParallelGroup(GroupLayout.Alignment.LEADING)
              .addComponent(labelApplicant)
              .addComponent(labelStatus)
              .addComponent(labelDate))
            .addGroup(createParallelGroup(GroupLayout.Alignment.TRAILING)
              .addComponent(fieldApplicant)
              .addComponent(dropdownStatus)
              .addComponent(pickerDate))
          )
          .addComponent(labelViolations)
          .addComponent(buttonSave)
      )
      setVerticalGroup(
        createSequentialGroup()
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(labelApplicant)
            .addComponent(fieldApplicant))
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(labelStatus)
            .addComponent(dropdownStatus))
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(labelDate)
            .addComponent(pickerDate))
          .addComponent(labelViolations)
          .addComponent(buttonSave)
      )
    }
  }

  private fun finishEditing(e: ActionEvent) {
    val request = EmploymentRequest(
      applicant = fieldApplicant.text,
      date = pickerDate.dateTimePermissive,
      status = dropdownStatus.selectedItem as EmploymentRequest.Status)

    validator.findViolations(request)
      ?.joinToString(separator = "</li><li>", prefix = "<html>$VALIDATION_FAILED<ul><li>", postfix = "</li></ul></html>")
      ?.let(labelViolations::setText)
      ?: triggerEditingFinishEvent(EditingFinishEvent(e, request))
  }
}
