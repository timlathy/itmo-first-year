package ru.ifmo.se.lab5

import org.jline.reader.Buffer
import org.jline.reader.Candidate
import org.jline.reader.LineReader
import org.jline.reader.ParsedLine
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.Mockito
import org.junit.jupiter.api.Assertions.assertEquals

@ExtendWith(MockitoExtension::class)
class JsonCompleterTest {
  private val completer = JsonCompleter(TestSerializable::class.java)

  @Test
  fun `suggests an opening brace at the start of a definition`() {
    assertCompletion("add ",
      "{\"someString\":",
      "{\"someInt\":",
      "{\"someEnum\":")
  }

  fun assertCompletion(line: String, vararg expected: String) {
    val parsed = Mockito.mock(ParsedLine::class.java)
    Mockito.`when`(parsed.word()).thenReturn(line.split(' ').last())
    assertCompletion(line, parsed, *expected)
  }

  fun assertCompletion(line: String, parsed: ParsedLine, vararg expected: String) {
    val buffer = Mockito.mock(Buffer::class.java)
    val reader = Mockito.mock(LineReader::class.java)
    Mockito.`when`(buffer.toString()).thenReturn(line)
    Mockito.`when`(reader.getBuffer()).thenReturn(buffer)

    val actual = mutableListOf<Candidate>()
      .apply { completer.complete(reader, parsed, this) }
      .map { it.value() }

    assertEquals(expected.sorted(), actual.sorted())
  }
}
