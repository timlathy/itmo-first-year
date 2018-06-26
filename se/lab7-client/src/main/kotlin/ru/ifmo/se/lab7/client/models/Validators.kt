package ru.ifmo.se.lab7.client.models

import javafx.scene.control.TextField
import ru.ifmo.se.lab7.client.LocaleControl
import tornadofx.*

class Validators {
  companion object {
    fun TextField.textContainingDoubleBetween(from: Double, to: Double) =
      validator {
        if (it?.isDouble() == false) doubleBetweenMessage(from, to)
        else it?.toDoubleOrNull()?.let { v ->
          if (v >= from && v <= to) null
          else doubleBetweenMessage(from, to)
        }
      }

    fun doubleBetweenMessage(from: Double, to: Double) =
      LocaleControl.string("validation.range")
        .replace("{from}", from.toString())
        .replace("{to}", to.toString())
        .let { message -> ValidationMessage(message, ValidationSeverity.Error) }
  }
}
