package ru.ifmo.se.lab6.client

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.jsonSchema.types.ObjectSchema
import com.fasterxml.jackson.module.kotlin.readValue
import org.jline.builtins.Completers
import org.jline.reader.impl.completer.StringsCompleter

data class ServerCommand(val name: String, val argument: ArgumentType) {
  enum class ArgumentType { JSON, FILE_PATH, NONE }

  fun exec(arg: String?, conn: ServerConnection) =
    unescape(conn.fetchResponse(name, arg))

  private fun unescape(str: String) =
    str.trim('"').replace("\\n", "\n")
}

class CommandRunner(private val commands: List<ServerCommand>,
                    private val argumentSchema: ObjectSchema,
                    private val connection: ServerConnection) {
  enum class CommandStatus { SUCCESS, ERROR }
  data class CommandResult(val message: String, val status: CommandStatus)

  companion object {
    private val mapper = ObjectMapper().apply { findAndRegisterModules() }

    fun initRunnerWithConnection(conn: ServerConnection) =
      CommandRunner(fetchCommands(conn), fetchSchema(conn), conn)

    data class SerializedCommand(val name: String = "", val argument: String = "")

    fun fetchCommands(conn: ServerConnection): List<ServerCommand> {
      val response = conn.fetchResponse("list_commands")
      val commands: Array<SerializedCommand> =
        mapper.readValue(response, mapper.typeFactory.constructArrayType(SerializedCommand::class.java))
      return commands.map {
        ServerCommand(it.name, when (it.argument) {
          "json" -> ServerCommand.ArgumentType.JSON
          else -> ServerCommand.ArgumentType.NONE
        })
      }
    }

    fun fetchSchema(conn: ServerConnection): ObjectSchema {
      val response = conn.fetchResponse("argument_schema")
      return mapper.readValue(response)
    }
  }

  fun eval(line: String): CommandResult =
    try {
      val parsed = line.trim().split(" ", limit = 2)
      val argument = if (parsed.size == 2) parsed.last() else null
      commands
        .find { cmd -> cmd.name == parsed.first() }
        ?.let { cmd -> CommandResult(cmd.exec(argument, connection), CommandStatus.SUCCESS) }
        ?: CommandResult("Unknown command \"${parsed.first()}\"", CommandStatus.ERROR)
    }
    catch (e: ServerConnection.RequestFailureException) {
      CommandResult(e.message, CommandStatus.ERROR)
    }

  fun constructCompleter(): Completers.RegexCompleter {
    val completionMap = hashMapOf(
      "JSON" to JsonCompleter(argumentSchema),
      "PATH" to Completers.FileNameCompleter(),
      *commands.mapIndexed { i, cmd -> "C$i" to StringsCompleter(cmd.name) }.toTypedArray()
    )
    val completionRegex = commands.mapIndexed { i, cmd -> when (cmd.argument) {
      ServerCommand.ArgumentType.JSON -> "C$i JSON+"
      ServerCommand.ArgumentType.FILE_PATH -> "C$i PATH"
      else -> "C$i"
    }}.joinToString(" | ")

    return Completers.RegexCompleter(completionRegex, { completionMap[it] })
  }
}
