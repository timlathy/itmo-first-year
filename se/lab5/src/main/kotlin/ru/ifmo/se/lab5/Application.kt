package ru.ifmo.se.lab5

import org.jline.terminal.TerminalBuilder
import org.jline.reader.*

fun main(args: Array<String>) {
  val terminal = TerminalBuilder.builder().build()
  val completer = CommandRunner.constructCompleter(Commands.list, Commands.elementClass)

  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .completer(completer)
    .parser(LineParser())
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
