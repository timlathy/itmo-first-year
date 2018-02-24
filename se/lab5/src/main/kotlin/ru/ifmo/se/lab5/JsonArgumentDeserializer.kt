package ru.ifmo.se.lab5

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import java.util.logging.Level
import java.util.logging.Logger
import javax.validation.Validation

import ru.ifmo.se.lab5.CommandRunner.CommandExecutionException

class JsonArgumentDeserializer<E>(private val elementClass: Class<E>) {
  private val mapper = ObjectMapper().apply {
    findAndRegisterModules()
  }

  private val validator by lazy {
    /* Turn off logging before initializing the validator */
    Logger.getLogger("org.hibernate").level = Level.OFF

    Validation.buildDefaultValidatorFactory().validator
  }

  fun fromString(jsonArgument: String) =
    try {
      val request: E = mapper.readValue(jsonArgument, elementClass)

      validator.validate(request)
        .takeIf { it.isNotEmpty() }
        ?.map { it.message }
        ?.sorted()
        ?.joinToString(", ")
        ?.let { violations -> throw CommandExecutionException(
          "The employment request specified is invalid: $violations")
        }

      request
    }
    catch (e: UnrecognizedPropertyException) {
      throw CommandExecutionException("Unknown field \"${e.propertyName}\". " +
        "Valid fields for employment requests are " +
        e.knownPropertyIds.joinToString(", ") { "\"" + it + "\"" })
    }
    catch (e: JsonProcessingException) {
      throw CommandExecutionException("Unable to read the employment request specified; " +
        "please make sure the data you are entering is a valid JSON")
    }
}