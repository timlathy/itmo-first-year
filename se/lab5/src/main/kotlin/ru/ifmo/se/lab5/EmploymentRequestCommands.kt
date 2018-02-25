package ru.ifmo.se.lab5

import ru.ifmo.se.lab5.CommandRunner.*
import java.util.PriorityQueue

typealias QueueCommand = Command<EmploymentRequest>
typealias CommandArg = Command.ArgumentType
typealias Deserializer = JsonArgumentDeserializer<EmploymentRequest>

class EmploymentRequestCommands: CommandList<EmploymentRequest> {
  override val list: List<QueueCommand> by lazy {
    val jsonDeserializer = Deserializer(EmploymentRequest::class.java)
    listOf(
      ClearCommand(),
      AddCommand(jsonDeserializer),
      RemoveLowerCommand(jsonDeserializer)
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

  class RemoveLowerCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "remove_lower"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      val element = deserializer.fromString(args)
      queue.removeAll { it < element }
    }
  }

  class RemoveLastCommand: QueueCommand {
    override val name = "remove_last"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      /* Since there's no easy (= without iterating the whole collection) way
       * to remove the tail of a PriorityQueue, we copy every element but the tail
       * to a temporary queue, and then fill the old one using it.
       */
      PriorityQueue(queue.comparator()).let { temp ->
        while (queue.size > 1) temp.add(queue.poll())
        queue.clear()
        queue.addAll(temp)
      }
    }
  }
}
