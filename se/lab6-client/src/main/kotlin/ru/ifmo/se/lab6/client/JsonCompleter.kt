package ru.ifmo.se.lab6.client

import org.jline.reader.*
import com.fasterxml.jackson.module.jsonSchema.*
import com.fasterxml.jackson.module.jsonSchema.types.ObjectSchema

class JsonCompleter(schema: ObjectSchema): Completer {
  private val props: Map<String, JsonSchema> = schema.properties

  override fun complete(r: LineReader, pl: ParsedLine, cs: MutableList<Candidate>) {
    /* jline's autocompletion works by splitting the line into words (delimited by spaces)
     * and matching each one with the specified completer.
     *
     * When the TAB key is hit, every word on the line is fed into a completer corresponding
     * to its position (in case of JsonCompleter, it's every word after the first one),
     * and if no candidates are returned, the completion is aborted.
     *
     * Therefore, we check if the word passed to us is a complete key or key-value pair,
     * and return it as-is (to be clear, this result is not shown to the user).
     */
    if (pl.word().endsWith(":") || pl.word().endsWith(",")) {
      cs.add(Candidate(pl.word()))
      return
    }

    /* If every word prior to the last one yields a candidate, the completer is called
     * again, this time to find the actual candidates shown to the user.
     *
     * The ParsedLine instance we receive is useless at this point; we need to
     * do a bit of our own parsing.
     */

    val line = r.buffer.toString()
    val words = if (line.contains("{"))
      line.substringAfterLast("{").split(",")
    else
      listOf("")
    val prefix = if (words.size == 1) "{\"" else "\""
    val typed = words.last().trimStart('"', ' ')

    props.keys
      .filter { it.startsWith(typed) }
      .forEach { cs.add(Candidate(prefix + it + "\":")) }
  }
}
