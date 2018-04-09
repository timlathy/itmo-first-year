package ru.ifmo.se.lab6.client

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import java.io.File
import java.io.IOException
import java.io.PrintWriter
import java.util.*

class CustomCommands {
  companion object {
    private val mapper = ObjectMapper().apply { findAndRegisterModules() }

    fun list(): List<ServerCommand> =
      listOf(SaveCommand(mapper), ImportCommand(mapper))
  }

  /**
   * Exports the current state of the queue to the given file.
   */
  class SaveCommand(private val mapper: ObjectMapper): ServerCommand("save", ArgumentType.FILE_PATH) {
    override fun exec(conn: ServerConnection, arg: String?, name: String): CommandRunner.CommandResult {
      val target = File(arg)
      try {
        target.createNewFile()
        if (!(target.canRead() && target.canWrite()))
          throw IOException("The specified file has to be readable and writable by the application.")
      } catch (e: IOException) {
        return CommandRunner.CommandResult(e.message ?:
          "An error has occurred while working with the specified file.", CommandRunner.CommandStatus.ERROR)
      }

      val (contents, _) = super.exec(conn, null, "dump_queue")

      val writer = PrintWriter(target)
      writer.write(contents)
      writer.close()

      return CommandRunner.CommandResult("The queue has been successfully saved.",
        CommandRunner.CommandStatus.SUCCESS)
    }
  }

  /**
   * Adds all elements from the specified file to the queue.
   */
  class ImportCommand(private val mapper: ObjectMapper): ServerCommand("import", ArgumentType.FILE_PATH) {
    override fun exec(conn: ServerConnection, arg: String?, name: String): CommandRunner.CommandResult {
      val target = File(arg)
      try {
        target.createNewFile()
        if (!target.canRead()) throw IOException("The specified file is not readable.")
      } catch (e: IOException) {
        return CommandRunner.CommandResult(e.message ?:
          "An error has occurred while reading the specified file.", CommandRunner.CommandStatus.ERROR)
      }

      val json = StringBuilder().apply {
        Scanner(target).let { scan ->
          while (scan.hasNext()) append(scan.nextLine())
          scan.close()
        }
        if (isEmpty()) append("[]")
      }.toString()

      val queue: Array<JsonNode> = with(mapper) {
        readValue(json, typeFactory.constructArrayType(JsonNode::class.java))
      }
      val result = queue.map(mapper::writeValueAsString).joinToString("\n") { element ->
        super.exec(conn, element, "add").message
      }

      return CommandRunner.CommandResult(result, CommandRunner.CommandStatus.SUCCESS)
    }
  }
}
