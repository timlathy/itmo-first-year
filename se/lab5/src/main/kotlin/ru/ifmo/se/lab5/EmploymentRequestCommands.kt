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
      AddIfMaxCommand(jsonDeserializer),
      AddIfMinCommand(jsonDeserializer),
      RemoveLowerCommand(jsonDeserializer),
      RemoveGreaterCommand(jsonDeserializer),
      RemoveFirstCommand(),
      RemoveLastCommand(),
      RemoveCommand(jsonDeserializer)
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

  class AddIfMaxCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "add_if_max"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      val element = deserializer.fromString(args)
      if (queue.peek() < element) queue.add(element)
    }
  }

  class AddIfMinCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "add_if_min"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      val element = deserializer.fromString(args)

      /* PriorityQueue does not provide a way to peek at the tail of the queue,
       * (#toArray does not respect queue order), so we have to fully traverse
       * a clone of it (so as not to remove elements from the source queue). */
      val tail = PriorityQueue(queue.comparator()).let { clone ->
        clone.addAll(queue)
        while (clone.size > 1) clone.poll()
        clone.poll()
      }

      if (element < tail) queue.add(element)
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

  class RemoveGreaterCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "remove_greater"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      val element = deserializer.fromString(args)
      queue.removeAll { it > element }
    }
  }

  class RemoveFirstCommand: QueueCommand {
    override val name = "remove_first"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      queue.poll()
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

  class RemoveCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "remove"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) {
      queue.remove(deserializer.fromString(args))
    }
  }
}
