package ru.ifmo.se.lab6.server

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.jsonSchema.JsonSchemaGenerator
import java.util.concurrent.PriorityBlockingQueue
import java.util.stream.Collectors

class EmploymentRequestCommands {
  companion object {
    const val STATUS_CLEARED = "The queue has been cleared"
    const val STATUS_ELEMENT_ADDED = "An element has been added to the queue"
    const val STATUS_UNCHANGED = "The queue has not been changed"
    const val STATUS_ONE_REMOVED = "One element has been removed from the queue"
    const val STATUS_MANY_REMOVED = "elements have been removed from the queue"

    private val mapper = ObjectMapper().apply { findAndRegisterModules() }

    val commandList: List<Command<EmploymentRequest>> = listOf(
      ClearCommand(),
      AddCommand(),
      AddIfMaxCommand(),
      AddIfMinCommand(),
      RemoveLowerCommand(),
      RemoveGreaterCommand(),
      RemoveFirstCommand(),
      RemoveLastCommand(),
      RemoveCommand(),
      RemoveAllCommand(),
      InfoCommand(),
      DumpQueueCommand(mapper),
      ArgumentSchemaCommand(mapper)
    )

    private inline fun<T> addIf(pred: (T) -> Boolean, element: T, queue: PriorityBlockingQueue<T>) =
      element.takeIf(pred)
             ?.let { queue.add(it); "$STATUS_ELEMENT_ADDED: $it" }
             ?: STATUS_UNCHANGED

    private fun<T> removeAll(pred: (T) -> Boolean, queue: PriorityBlockingQueue<T>) =
      queue.let {
        val sizeBefore = queue.size
        it.removeAll(pred)
        val sizeDiff = sizeBefore - queue.size

        when {
          sizeDiff == 1 -> STATUS_ONE_REMOVED
          sizeDiff > 1 -> "$sizeDiff $STATUS_MANY_REMOVED"
          else -> STATUS_UNCHANGED
        }
      }
  }

  /**
   * Clears the queue. Returns success regardless of whether
   * there were elements in a queue or not.
   */
  class ClearCommand: CommandWithoutArgument<EmploymentRequest> {
    override val name = "clear"

    override fun exec(queue: PriorityBlockingQueue<EmploymentRequest>) =
      queue.clear().run { STATUS_CLEARED }
  }

  /**
   * Adds a given element to the queue.
   */
  class AddCommand: Command<EmploymentRequest> {
    override val name = "add"

    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      addIf({ true }, arg, queue)
  }

  /**
   * Adds a given element to the queue if it has higher priority
   * than the head of the queue.
   */
  class AddIfMaxCommand: Command<EmploymentRequest> {
    override val name = "add_if_max"

    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      addIf({ it > queue.peek() }, arg, queue)
  }

  /**
   * Adds a given element to the queue if it has lower priority
   * than the tail of the queue.
   */
  class AddIfMinCommand: Command<EmploymentRequest> {
    override val name = "add_if_min"

    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      /* PriorityBlockingQueue does not provide a way to peek at the tail of the queue,
       * (#toArray does not respect queue order), so we have to fully traverse
       * a clone of it (so as not to remove elements from the source queue). */
      PriorityBlockingQueue(queue.size, queue.comparator())
        .let { clone ->
          clone.addAll(queue)
          while (clone.size > 1) clone.poll()
          clone.poll()
        }
        .let { tailEl -> addIf({ it < tailEl }, arg, queue) }
  }

  /**
   * Removes all elements with the lower priority than the given one.
   * Returned status includes the number of elements removed.
   */
  class RemoveLowerCommand: Command<EmploymentRequest> {
    override val name = "remove_lower"

    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      removeAll({ it < arg }, queue)
  }

  /**
   * Removes all elements with the higher priority than the given one.
   * Returned status includes the number of elements removed.
   */
  class RemoveGreaterCommand: Command<EmploymentRequest> {
    override val name = "remove_greater"

    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      removeAll({ it > arg }, queue)
  }

  /**
   * Removes the head of the queue.
   * Returns a neutral status if there were no elements in the queue.
   */
  class RemoveFirstCommand: CommandWithoutArgument<EmploymentRequest> {
    override val name = "remove_first"
    
    override fun exec(queue: PriorityBlockingQueue<EmploymentRequest>) =
      if (queue.poll() != null) STATUS_ONE_REMOVED else STATUS_UNCHANGED
  }

  /**
   * Removes the tail of the queue.
   * Returns a neutral status if there were no elements in the queue.
   */
  class RemoveLastCommand: CommandWithoutArgument<EmploymentRequest> {
    override val name = "remove_last"
    
    override fun exec(queue: PriorityBlockingQueue<EmploymentRequest>) =
      /* Since there's no easy (= without iterating the whole collection) way
       * to remove the tail of a PriorityBlockingQueue, we copy every element but the tail
       * to a temporary queue, and then fill the old one using it.
       */
      if (queue.size == 0) STATUS_UNCHANGED
      else PriorityBlockingQueue(queue.size, queue.comparator()).let { temp ->
        while (queue.size > 1) temp.add(queue.poll())
        queue.clear()
        queue.addAll(temp)
        STATUS_ONE_REMOVED
      }
  }

  /**
   * Removes the first element equivalent to the given one.
   * Returns a neutral status if no elements were removed.
   */
  class RemoveCommand: Command<EmploymentRequest> {
    override val name = "remove"
    
    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      if (queue.remove(arg)) STATUS_ONE_REMOVED else STATUS_UNCHANGED
  }

  /**
   * Removes all elements equal to the given one.
   * Returns the number of elements removed.
   */
  class RemoveAllCommand: Command<EmploymentRequest> {
    override val name = "remove_all"
    
    override fun exec(arg: EmploymentRequest, queue: PriorityBlockingQueue<EmploymentRequest>) =
      removeAll({ it == arg }, queue)
  }

  /**
   * Returns basic information (type, elements) for the queue.
   */
  class InfoCommand: CommandWithoutArgument<EmploymentRequest> {
    override val name = "info"
    
    override fun exec(queue: PriorityBlockingQueue<EmploymentRequest>) =
      StringBuilder().apply {
        appendln("=== Queue information")
        appendln("Type:")
        appendln("  ${queue.javaClass.canonicalName}")
        appendln("Elements:")
        if (queue.isEmpty()) appendln("  (none)")
        else append(queue.stream()
          .sorted(queue.comparator())
          .map { it: EmploymentRequest -> "  * $it" }
          .collect(Collectors.joining("\n")))
        append("\n===")
      }.toString()
  }

  /**
   * Returns all elements currently present in the queue.
   */
  class DumpQueueCommand(private val mapper: ObjectMapper): CommandWithoutArgument<EmploymentRequest> {
    override val name = "dump_queue"

    override fun exec(queue: PriorityBlockingQueue<EmploymentRequest>) =
      queue.stream().sorted(queue.comparator()).collect(Collectors.toList())
  }

  /**
   * Returns a JSON schema of the argument (EmploymentRequest).
   */
  class ArgumentSchemaCommand(private val mapper: ObjectMapper): CommandWithoutArgument<EmploymentRequest> {
    override val name = "argument_schema"

    override fun exec(queue: PriorityBlockingQueue<EmploymentRequest>) =
        JsonSchemaGenerator(mapper)
          .generateSchema(EmploymentRequest::class.java)
          .asObjectSchema()
  }
}
