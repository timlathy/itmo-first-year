package ru.ifmo.se.lab5

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import java.util.*

import ru.ifmo.se.lab5.CommandRunner.*

typealias QueueCommand = Command<EmploymentRequest>
typealias CommandArg = Command.ArgumentType
typealias Deserializer = JsonArgumentDeserializer<EmploymentRequest>

class EmploymentRequestCommands: CommandList<EmploymentRequest> {
  override val list: List<QueueCommand> by lazy {
    val jsonDeserializer = Deserializer(EmploymentRequest::class.java)
    listOf(
      ClearCommand(),
      AddCommand(jsonDeserializer)
    )
  }

  override val elementClass = EmploymentRequest::class.java

  class ClearCommand: QueueCommand {
    override val name = "clear"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      queue.clear()
  }

  class AddCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "add"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      queue.add(deserializer.fromString(args))
    }
  }
}
