package ru.ifmo.se.lab5

import org.jline.terminal.TerminalBuilder
import org.jline.builtins.Completers.FileNameCompleter

import org.jline.reader.*
import org.jline.reader.impl.DefaultParser
import org.jline.builtins.Completers
import org.jline.reader.impl.completer.StringsCompleter

fun main(args: Array<String>) {
  val terminal = TerminalBuilder.builder().build()

  val completion = hashMapOf(
    "C0" to StringsCompleter("load"),
    "C1" to StringsCompleter("add_if_min"),
    "JSON" to JsonCompleter(EmploymentRequest::class.java),
    "C2" to StringsCompleter("load"),
    "PATH" to FileNameCompleter())
  val completer = Completers.RegexCompleter(
    "C0 | C1 JSON+ | C2 PATH", { completion[it] })

  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .completer(completer)
    .parser(object : DefaultParser() {
      override fun parse(line: String?, cursor: Int, context: Parser.ParseContext?): ParsedLine {
          val line = line ?: ""
          val words = line.split(" ")

          return if (cursor == line.length) {

            ArgumentList(line, words, (words.size - 1), words.last().length, cursor)
          }
          else {
            var position = 0
            val atWord = words.find {
              position += it.length + 1
              cursor < position
            } ?: ""
            ArgumentList(line, words, words.indexOf(atWord), atWord.length, cursor)
          }
        }
    })
    .build()

  while (true) {
    val line = try {
      reader.readLine(/* prompt */ "> ", null, null as MaskingCallback?, null)
    }
    catch (e: UserInterruptException) {
      println("Exit with ^D")
      continue
    }
    catch (e: Exception) {
      break
    }

    terminal.writer().println("===>" + line)
    terminal.flush()
  }
}
