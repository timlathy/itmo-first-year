package ru.ifmo.se.lab5

import java.util.*

typealias QueueCommand = CommandRunner.Command<EmploymentRequest>
typealias CommandArg = CommandRunner.Command.ArgumentType

class EmploymentRequestCommands: CommandRunner.CommandList<EmploymentRequest> {
  override val list: List<QueueCommand> = listOf(
    ClearCommand(), AddIfMinCommand())

  override val elementClass = EmploymentRequest::class.java

  class ClearCommand: QueueCommand {
    override val name = "clear"
    override val argument = CommandArg.NONE

    override fun run(queue: PriorityQueue<EmploymentRequest>) =
      queue.clear()
  }

  class AddIfMinCommand: QueueCommand {
    override val name = "add_if_min"
    override val argument = CommandArg.JSON

    override fun run(queue: PriorityQueue<EmploymentRequest>) =
      queue.clear()
  }
}