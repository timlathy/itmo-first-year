package ru.ifmo.se.lab7.server

import java.util.concurrent.PriorityBlockingQueue

typealias CollectionChanges<E> = List<CollectionChange<E>>
typealias CommandExecutionListener<E> = (commandName: String, changes: CollectionChanges<E>) -> Unit
typealias CommandResult<E> = Pair<Any, CollectionChanges<E>>

data class CollectionChange<E>(val element: E, val type: ChangeType) {
  enum class ChangeType { ADDITION, REMOVAL }
}

interface Command<E> {
  val name: String
  val argument: String get() = "json"

  fun exec(arg: E, queue: PriorityBlockingQueue<E>): CommandResult<E>
}

interface CommandWithoutArgument<E> : Command<E> {
  override val argument: String get() = "none"

  fun exec(queue: PriorityBlockingQueue<E>): CommandResult<E>

  override fun exec(arg: E, queue: PriorityBlockingQueue<E>) = exec(queue)
}

class CommandRunner<E>(private val commands: List<Command<E>>,
                       private val queue: PriorityBlockingQueue<E>) {
  class MissingArgumentException: Exception()
  class UnknownCommandException: Exception()

  private val listeners = mutableListOf<CommandExecutionListener<E>>()

  fun addCommandListener(listener: CommandExecutionListener<E>) =
    listeners.add(listener)

  fun eval(commandName: String, argument: E?) =
    if (commandName == "list_commands") commands
    else commands
      .find { it.name == commandName }
      ?.let {
        if (it is CommandWithoutArgument) it.exec(queue)
        else if (argument != null) it.exec(argument, queue)
        else throw MissingArgumentException()
      }
      ?.let { (result, changes) ->
        listeners.forEach { it(commandName, changes) }
        result
      }
      ?: throw UnknownCommandException()
}
