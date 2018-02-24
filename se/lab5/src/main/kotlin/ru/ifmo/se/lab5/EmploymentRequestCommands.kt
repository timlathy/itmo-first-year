package ru.ifmo.se.lab5

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import com.fasterxml.jackson.module.kotlin.readValue
import javax.validation.Validation
import java.util.*

import ru.ifmo.se.lab5.CommandRunner.*

typealias QueueCommand = Command<EmploymentRequest>
typealias CommandArg = Command.ArgumentType

class EmploymentRequestCommands: CommandList<EmploymentRequest> {
  override val list: List<QueueCommand> = listOf(
    ClearCommand(), AddCommand())

  override val elementClass = EmploymentRequest::class.java

  class ClearCommand: QueueCommand {
    override val name = "clear"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      queue.clear()
  }

  class AddCommand: QueueCommand {
    override val name = "add"
    override val argument = CommandArg.JSON

    private val mapper = ObjectMapper().apply { findAndRegisterModules() }
    private val validator = Validation.buildDefaultValidatorFactory().validator

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      try {
        val request: EmploymentRequest = mapper.readValue(args)

        validator.validate(request)
          .takeIf { it.isNotEmpty() }
          ?.joinToString(", ") { it.message }
          ?.let { violations -> throw CommandExecutionException(
            "The employment request specified is invalid: $violations") }

        queue.add(request)
      }
      catch (e: UnrecognizedPropertyException) {
        throw CommandExecutionException("Unknown field \"${e.propertyName}\". " +
          "Valid fields for employment requests are " +
          e.knownPropertyIds.joinToString(", ") { "\"" + it + "\"" })
      }
      catch (e: JsonProcessingException) {
        throw CommandExecutionException("Unable to read the employment request specified. " +
          "Please make sure the data you are entering is a valid JSON.")
      }
    }
  }
}