package ru.ifmo.se.lab7.client.models

import javafx.util.converter.NumberStringConverter
import java.lang.RuntimeException
import java.text.ParseException

class Converters {
  companion object {
    /**
     * Returns `null` instead of throwing a `RuntimeException`
     * if the number cannot be parsed.
     */
    class SafeNumberStringConverter: NumberStringConverter() {
      override fun fromString(value: String?): Number? =
        try {
          super.fromString(value)
        }
        catch(e: RuntimeException) {
          /* Thanks for wrapping it */
          if (e.cause is ParseException) null
          else throw e
        }
    }
  }
}
