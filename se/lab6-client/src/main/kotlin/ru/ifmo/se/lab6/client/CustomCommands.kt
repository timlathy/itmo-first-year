package ru.ifmo.se.lab6.client

import com.fasterxml.jackson.databind.ObjectMapper
import java.io.File
import java.io.IOException
import java.io.PrintWriter

class CustomCommands {
  companion object {
    private val mapper = ObjectMapper().apply { findAndRegisterModules() }

    fun list(): List<ServerCommand> =
      listOf(SaveCommand(mapper))
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
}
