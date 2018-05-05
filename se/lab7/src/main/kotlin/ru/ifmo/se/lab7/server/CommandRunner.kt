package ru.ifmo.se.lab7.server

import java.util.concurrent.PriorityBlockingQueue

interface Command<E> {
  val name: String
  val argument: String get() = "json"

  fun exec(arg: E, queue: PriorityBlockingQueue<E>): Any
}

interface CommandWithoutArgument<E> : Command<E> {
  override val argument: String get() = "none"

  fun exec(queue: PriorityBlockingQueue<E>): Any

  override fun exec(arg: E, queue: PriorityBlockingQueue<E>) = exec(queue)
}

class CommandRunner<E>(private val commands: List<Command<E>>,
                       private val queue: PriorityBlockingQueue<E>) {
  class MissingArgumentException: Exception()
  class UnknownCommandException: Exception()

  fun eval(commandName: String, argument: E?) =
    if (commandName == "list_commands") commands
    else commands
      .find { it.name == commandName }
      ?.let { if (it is CommandWithoutArgument) it.exec(queue)
              else if (argument != null) it.exec(argument, queue)
              else throw MissingArgumentException() }
      ?: throw UnknownCommandException()
}
