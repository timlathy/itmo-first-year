package ru.ifmo.se.lab5

import ru.ifmo.se.lab5.CommandRunner.*
import java.util.PriorityQueue

import ru.ifmo.se.lab5.CommandRunner.Command.CommandStatus
import ru.ifmo.se.lab5.CommandRunner.Command.CommandStatus.*

typealias QueueStorage = PriorityQueueStorage<EmploymentRequest>
typealias QueueCommand = Command<EmploymentRequest>
typealias CommandArg = Command.ArgumentType
typealias Deserializer = JsonArgumentDeserializer<EmploymentRequest>

class EmploymentRequestCommands(private val storage: QueueStorage): CommandList<EmploymentRequest> {
  companion object {
    const val STATUS_CLEARED = "The queue has been cleared"
    const val STATUS_ELEMENT_ADDED = "An element has been added to the queue"
    const val STATUS_UNCHANGED = "The queue has not been changed"
    const val STATUS_ONE_REMOVED = "One element has been removed from the queue"
    const val STATUS_MANY_REMOVED = "elements have been removed from the queue"
    const val STATUS_LOADED = "The queue has been reloaded"
    const val STATUS_SAVED = "The queue has been saved"

    private inline fun<T> addIf(element: T, pred: (T) -> Boolean, queue: PriorityQueue<T>) =
      if (pred(element)) {
        queue.add(element)
        SuccessStatus("$STATUS_ELEMENT_ADDED: $element")
      }
      else NeutralStatus(STATUS_UNCHANGED)

    private fun<T> removeAll(pred: (T) -> Boolean, queue: PriorityQueue<T>) =
      queue.let {
        val sizeBefore = queue.size
        it.removeAll(pred)
        val sizeDiff = sizeBefore - queue.size

        when {
          sizeDiff == 1 -> SuccessStatus(STATUS_ONE_REMOVED)
          sizeDiff > 1 -> SuccessStatus("$sizeDiff $STATUS_MANY_REMOVED")
          else -> NeutralStatus(STATUS_UNCHANGED)
        }
      }
  }

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
      RemoveCommand(jsonDeserializer),
      InfoCommand(),
      LoadCommand(storage),
      SaveCommand(storage)
    )
  }

  override val elementClass = EmploymentRequest::class.java

  class ClearCommand: QueueCommand {
    override val name = "clear"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>): CommandStatus {
      queue.clear()
      return SuccessStatus(STATUS_CLEARED)
    }
  }

  class AddCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "add"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      addIf(deserializer.fromString(args), { true }, queue)
  }

  class AddIfMaxCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "add_if_max"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      addIf(deserializer.fromString(args), { it > queue.peek() }, queue)
  }

  class AddIfMinCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "add_if_min"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>): CommandStatus {
      val element = deserializer.fromString(args)

      /* PriorityQueue does not provide a way to peek at the tail of the queue,
       * (#toArray does not respect queue order), so we have to fully traverse
       * a clone of it (so as not to remove elements from the source queue). */
      val tail = PriorityQueue(queue.comparator()).let { clone ->
        clone.addAll(queue)
        while (clone.size > 1) clone.poll()
        clone.poll()
      }

      return addIf(element, { it < tail }, queue)
    }
  }

  class RemoveLowerCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "remove_lower"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      deserializer.fromString(args).let { element -> removeAll({ it < element}, queue) }
  }

  class RemoveGreaterCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "remove_greater"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      deserializer.fromString(args).let { element -> removeAll({ it > element}, queue) }
  }

  class RemoveFirstCommand: QueueCommand {
    override val name = "remove_first"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      queue.poll()?.let { SuccessStatus(STATUS_ONE_REMOVED) } ?: NeutralStatus(STATUS_UNCHANGED)
  }

  class RemoveLastCommand: QueueCommand {
    override val name = "remove_last"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      /* Since there's no easy (= without iterating the whole collection) way
       * to remove the tail of a PriorityQueue, we copy every element but the tail
       * to a temporary queue, and then fill the old one using it.
       */
      if (queue.size == 0) NeutralStatus(STATUS_UNCHANGED)
      else PriorityQueue(queue.comparator()).let { temp ->
        while (queue.size > 1) temp.add(queue.poll())
        queue.clear()
        queue.addAll(temp)
        SuccessStatus(STATUS_ONE_REMOVED)
      }
  }

  class RemoveCommand(private val deserializer: Deserializer): QueueCommand {
    override val name = "remove"
    override val argument = CommandArg.JSON

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      if (queue.remove(deserializer.fromString(args))) SuccessStatus(STATUS_ONE_REMOVED)
      else NeutralStatus(STATUS_UNCHANGED)
  }

  class InfoCommand: QueueCommand {
    override val name = "info"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      StringBuilder().apply {
        appendln("=== Queue information")
        appendln("Type:")
        appendln("  ${queue.javaClass.canonicalName}")
        appendln("Elements:")
        PriorityQueue(queue.comparator()).apply {
          if (queue.isEmpty()) appendln("  (none)")
          else {
            addAll(queue)
            for (i in 1..queue.size) appendln("  $i. ${poll()}")
          }
        }
        append("===")
      }.let {
        NeutralStatus(it.toString())
      }
  }

  class LoadCommand(private val storage: QueueStorage): QueueCommand {
    override val name = "load"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      storage.read().let { loaded ->
        queue.clear()
        queue.addAll(loaded)
      SuccessStatus(STATUS_LOADED)
    }
  }

  class SaveCommand(private val storage: QueueStorage): QueueCommand {
    override val name = "save"
    override val argument = CommandArg.NONE

    override fun run(args: String, queue: PriorityQueue<EmploymentRequest>) =
      storage.write(queue).let { SuccessStatus(STATUS_SAVED) }
  }
}
