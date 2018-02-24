package ru.ifmo.se.lab5

import org.jline.reader.*
import org.jline.reader.impl.DefaultParser
import org.jline.terminal.TerminalBuilder
import java.util.*

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
        println("Use :quit or Ctrl-D (EOF) to exit")
        continue
      }
      catch (e: Exception) {
        break
      }

      if (line.isBlank()) continue

      when(line) {
        ":q", ":quit" -> return
        ":h", ":help" -> {
          terminal.writer().println("h")
        }
        else -> try {
          runner.eval(line, queue)
        }
        catch (e: CommandRunner.UnknownCommandException) {
          println("Unknown command ${e.command}. Enter :help for help")
        }
      }

      terminal.flush()
    }
  }

  class LineParser: DefaultParser() {
    override fun parse(line: String?, cursor: Int, context: Parser.ParseContext?): ParsedLine {
      val line = line ?: ""
      val words = line.split(" ")

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