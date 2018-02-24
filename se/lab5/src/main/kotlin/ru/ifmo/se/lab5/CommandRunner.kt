package ru.ifmo.se.lab5

import org.jline.builtins.Completers
import org.jline.reader.impl.completer.StringsCompleter
import java.util.*

class CommandRunner<E>(private val commands: CommandList<E>) {
  interface Command<E> {
    val name: String
    val argument: ArgumentType

    fun run(args: String, queue: PriorityQueue<E>)

    enum class ArgumentType {
      JSON,
      FILE_PATH,
      NONE
    }
  }

  interface CommandList<E> {
    val list: List<Command<E>>
    val elementClass: Class<E>
  }

  class CommandExecutionException(message: String): RuntimeException(message)
  class UnknownCommandException(val command: String): IllegalArgumentException()

  fun eval(line: String, queue: PriorityQueue<E>) {
    val parsed = line.split(" ", limit = 2)

    val command = commands.list.find { cmd -> cmd.name == parsed.first() } ?:
      throw UnknownCommandException(parsed.first())

    command.run(parsed.last(), queue)
  }

  fun constructCompleter(): Completers.RegexCompleter {
    val completionMap = hashMapOf(
      "JSON" to JsonCompleter(commands.elementClass),
      "PATH" to Completers.FileNameCompleter(),
      *commands.list.mapIndexed { i, cmd -> "C$i" to StringsCompleter(cmd.name) }.toTypedArray()
    )
    val completionRegex = commands.list.mapIndexed { i, cmd -> when (cmd.argument) {
      Command.ArgumentType.JSON -> "C$i JSON+"
      Command.ArgumentType.FILE_PATH -> "C$i PATH"
      else -> "C$i"
    }}.joinToString(" | ")

    return Completers.RegexCompleter(completionRegex, { completionMap[it] })
  }
}