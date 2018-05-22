package ru.ifmo.se.lab7.client.models

import javafx.scene.control.TextField
import tornadofx.*

class Validators {
  companion object {
    fun TextField.textContainingDoubleBetween(from: Double, to: Double) =
      validator {
        if (it?.isDouble() == false)
          ValidationMessage("The specified value should be in the range of ${from} to ${to}", ValidationSeverity.Error)
        else it?.toDoubleOrNull()?.let { v ->
          if (v >= from && v <= to) null
          else ValidationMessage("The specified value should be in the range of ${from} to ${to}", ValidationSeverity.Error)
        }
      }
  }
}
