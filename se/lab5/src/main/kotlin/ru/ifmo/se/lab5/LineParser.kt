package ru.ifmo.se.lab5

import org.jline.reader.ParsedLine
import org.jline.reader.Parser
import org.jline.reader.impl.DefaultParser

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