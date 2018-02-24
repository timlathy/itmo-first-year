package ru.ifmo.se.lab5

import org.jline.builtins.Completers
import org.jline.reader.impl.completer.StringsCompleter
import java.util.*

class CommandRunner<E>(private val commands: CommandList<E>) {
  interface Command<E> {
    val name: String
    val argument: ArgumentType

    fun run(queue: PriorityQueue<E>)

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