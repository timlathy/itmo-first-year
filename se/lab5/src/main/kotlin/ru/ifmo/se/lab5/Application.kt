package ru.ifmo.se.lab5

import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder
import org.jline.builtins.Completers.FileNameCompleter
import org.jline.builtins.Completers.TreeCompleter

import org.jline.reader.*

import org.jline.builtins.Completers.TreeCompleter.node

fun main(args: Array<String>) {
  val terminal = TerminalBuilder.builder().build()

  var completer = TreeCompleter(
    node("load"),
    node("add_if_min",
      node(object : Completer {
        override fun complete(r: LineReader, line: ParsedLine, cs: MutableList<Candidate>) {
          cs.add(Candidate("json"))
        }
      })),
    node("import", node(FileNameCompleter()))
  )

  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .completer(completer)
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
