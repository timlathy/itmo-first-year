package ru.ifmo.se.lab5

import org.jline.reader.*
import org.jline.reader.impl.DefaultParser
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedStringBuilder
import org.jline.utils.AttributedStyle
import java.util.*
import ru.ifmo.se.lab5.CommandRunner.Command.CommandStatus.*

class Repl<E>(private val runner: CommandRunner<E>, private val queue: PriorityQueue<E>) {
  private val terminal = TerminalBuilder.builder().build()

  private val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .parser(LineParser())
    .completer(runner.constructCompleter())
    .build()

  fun loop() {
    while (true) {
      val line = try {
        reader.readLine("> ", null, null as MaskingCallback?, null)
      }
      catch (e: UserInterruptException) {
        terminal.writer().println("Use :quit or Ctrl-D (EOF) to exit")
        terminal.flush()
        continue
      }
      catch (e: Exception) {
        break
      }

      if (line.isBlank()) continue

      when(line) {
        ":q", ":quit" -> return
        ":h", ":help" -> {
          terminal.writer().println("WIP")
        }
        else -> try {
          val status = runner.eval(line, queue)

          terminal.writer().println(when (status) {
            is SuccessStatus -> styledString(status.message, AttributedStyle.GREEN)
            is NeutralStatus -> status.message
          })
        }
        catch (e: CommandRunner.UnknownCommandException) {
          terminal.writer().println(styledString(
            "Unknown command ${e.command}. Enter :help for help", AttributedStyle.RED))
        }
        catch (e: CommandRunner.CommandExecutionException) {
          terminal.writer().println(styledString(e.message ?: "", AttributedStyle.RED))
        }
        catch (e: Exception) {
          terminal.writer().println(styledString(
            "An error has occurred while executing your command. " +
              "Please ensure the data you're entering is correct.", AttributedStyle.RED))
        }
      }

      terminal.flush()
    }
  }

  private fun styledString(message: String, color: Int) =
    AttributedStringBuilder()
      .style(AttributedStyle.DEFAULT.foreground(color))
      .append(message)
      .style(AttributedStyle.DEFAULT).toAnsi()

  class LineParser: DefaultParser() {
    override fun parse(line: String?, cursor: Int, context: Parser.ParseContext?): ParsedLine {
      val line = line ?: ""
      val words = line.split(" ")

      if (line.endsWith("\\"))
        throw EOFError(1, line.length, "Multiline input")

      return if (cursor == line.length) {
        ArgumentList(line, words, (words.size - 1), words.last().length, cursor)
      } else {
        var position = 0
        val atWord = words.find {
          position += it.length + 1
          cursor < position
        } ?: ""
        ArgumentList(line, words, words.indexOf(atWord), atWord.length, cursor)
      }
    }
  }
}
