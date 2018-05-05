package ru.ifmo.se.lab7.server

import ru.ifmo.se.lab7.server.CollectionChange.ChangeType.ADDITION
import ru.ifmo.se.lab7.server.CollectionChange.ChangeType.REMOVAL
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.jsonSchema.JsonSchemaGenerator
import java.util.concurrent.PriorityBlockingQueue
import java.util.stream.Collectors

typealias ER = EmploymentRequest

class EmploymentRequestCommands {
  companion object {
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
             ?.let {
               queue.add(it)
               Pair("$STATUS_ELEMENT_ADDED: $it", listOf(CollectionChange(it, ADDITION)))
             }
             ?: Pair(STATUS_UNCHANGED, emptyList())

    private inline fun<T> withoutChanges(block: () -> Any): CommandResult<T>
      = Pair(block(), listOf())

    private fun<T> removeAll(pred: (T) -> Boolean, queue: PriorityBlockingQueue<T>) =
      with(queue) {
        val changes = mutableListOf<CollectionChange<T>>()

        with (iterator()) {
          while (hasNext()) {
            val element = next()
            if (pred(element)) {
              changes.add(CollectionChange(element, REMOVAL))
              remove()
            }
          }
        }

        val status = when (changes.size) {
          0 -> STATUS_UNCHANGED
          1 -> STATUS_ONE_REMOVED
          else -> "${changes.size} $STATUS_MANY_REMOVED"
        }

        Pair(status, changes.toList())
      }
  }

  /**
   * Clears the queue. Returns success regardless of whether
   * there were elements in a queue or not.
   */
  class ClearCommand: CommandWithoutArgument<ER> {
    override val name = "clear"

    override fun exec(queue: PriorityBlockingQueue<ER>) =
      removeAll({ true }, queue)
  }

  /**
   * Adds a given element to the queue.
   */
  class AddCommand: Command<ER> {
    override val name = "add"

    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>) =
      addIf({ true }, arg, queue)
  }

  /**
   * Adds a given element to the queue if it has higher priority
   * than the head of the queue.
   */
  class AddIfMaxCommand: Command<ER> {
    override val name = "add_if_max"

    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>) =
      addIf({ it > queue.peek() }, arg, queue)
  }

  /**
   * Adds a given element to the queue if it has lower priority
   * than the tail of the queue.
   */
  class AddIfMinCommand: Command<ER> {
    override val name = "add_if_min"

    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>) =
      /* PriorityBlockingQueue does not provide a way to peek at the tail of the queue,
       * (#toArray does not respect queue order), so getting the last element
       * is only possible if we sort it ourselves. */
      queue.stream()
        .sorted(queue.comparator().reversed())
        .findFirst().orElse(null)
        ?.let { tail -> addIf({ it < tail }, arg, queue) }
        ?: addIf({ true }, arg, queue)
  }

  /**
   * Removes all elements with the lower priority than the given one.
   * Returned status includes the number of elements removed.
   */
  class RemoveLowerCommand: Command<ER> {
    override val name = "remove_lower"

    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>) =
      removeAll({ it < arg }, queue)
  }

  /**
   * Removes all elements with the higher priority than the given one.
   * Returned status includes the number of elements removed.
   */
  class RemoveGreaterCommand: Command<ER> {
    override val name = "remove_greater"

    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>) =
      removeAll({ it > arg }, queue)
  }

  /**
   * Removes the head of the queue.
   * Returns a neutral status if there were no elements in the queue.
   */
  class RemoveFirstCommand: CommandWithoutArgument<ER> {
    override val name = "remove_first"
    
    override fun exec(queue: PriorityBlockingQueue<ER>) =
      queue.poll()
        ?.let { removed -> Pair(STATUS_ONE_REMOVED, listOf(CollectionChange(removed, REMOVAL))) }
        ?: Pair(STATUS_UNCHANGED, listOf())
  }

  /**
   * Removes the tail of the queue.
   * Returns a neutral status if there were no elements in the queue.
   */
  class RemoveLastCommand: CommandWithoutArgument<ER> {
    override val name = "remove_last"
    
    override fun exec(queue: PriorityBlockingQueue<ER>) =
      /* Since there's no easy (= without iterating the whole collection) way
       * to remove the tail of a PriorityBlockingQueue, we copy every element but the tail
       * to a temporary queue, and then fill the old one using it.
       */
      if (queue.size == 0) Pair(STATUS_UNCHANGED, listOf())
      else with(queue) {
        PriorityBlockingQueue(queue.size, queue.comparator()).let { temp ->
          while (size > 1) temp.add(poll())
          val removed = poll()

          addAll(temp)
          Pair(STATUS_ONE_REMOVED, listOf(CollectionChange(removed, REMOVAL)))
        }
      }
  }

  /**
   * Removes the first element equivalent to the given one.
   * Returns a neutral status if no elements were removed.
   */
  class RemoveCommand: Command<ER> {
    override val name = "remove"
    
    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>): CommandResult<ER> =
      queue.find { it == arg }
        ?.let { queue.remove(arg); Pair(STATUS_ONE_REMOVED, listOf(CollectionChange(it, REMOVAL))) }
        ?: Pair(STATUS_UNCHANGED, listOf())
  }

  /**
   * Removes all elements equal to the given one.
   * Returns the number of elements removed.
   */
  class RemoveAllCommand: Command<ER> {
    override val name = "remove_all"
    
    override fun exec(arg: ER, queue: PriorityBlockingQueue<ER>) =
      removeAll({ it == arg }, queue)
  }

  /**
   * Returns basic information (type, elements) for the queue.
   */
  class InfoCommand: CommandWithoutArgument<ER> {
    override val name = "info"
    
    override fun exec(queue: PriorityBlockingQueue<ER>): CommandResult<ER> = withoutChanges {
      StringBuilder().apply {
        appendln("=== Queue information")
        appendln("Type:")
        appendln("  ${queue.javaClass.canonicalName}")
        appendln("Elements:")
        if (queue.isEmpty()) appendln("  (none)")
        else append(queue.stream()
          .sorted(queue.comparator())
          .map { it: ER -> "  * $it" }
          .collect(Collectors.joining("\n")))
        append("\n===")
      }.toString()
    }
  }

  /**
   * Returns all elements currently present in the queue.
   */
  class DumpQueueCommand(private val mapper: ObjectMapper): CommandWithoutArgument<ER> {
    override val name = "dump_queue"

    override fun exec(queue: PriorityBlockingQueue<ER>): CommandResult<ER> = withoutChanges {
      queue.stream().sorted(queue.comparator()).collect(Collectors.toList())
    }
  }

  /**
   * Returns a JSON schema of the argument (EmploymentRequest).
   */
  class ArgumentSchemaCommand(private val mapper: ObjectMapper): CommandWithoutArgument<ER> {
    override val name = "argument_schema"

    override fun exec(queue: PriorityBlockingQueue<ER>): CommandResult<ER> = withoutChanges {
      JsonSchemaGenerator(mapper)
        .generateSchema(EmploymentRequest::class.java)
        .asObjectSchema()
    }
  }
}
