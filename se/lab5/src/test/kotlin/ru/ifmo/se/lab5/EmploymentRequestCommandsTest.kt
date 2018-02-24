package ru.ifmo.se.lab5

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.time.LocalDateTime
import java.util.*

import ru.ifmo.se.lab5.EmploymentRequestCommands.*

class EmploymentRequestCommandsTest {
  val jsonDeserializer = Deserializer(EmploymentRequest::class.java)

  @Test
  fun `"clear" removes all elements`() {
    val queue = PriorityQueue(listOf(EmploymentRequest("name", LocalDateTime.now())))
    assertTrue(queue.isNotEmpty())

    ClearCommand().run("", queue)
    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"add" inserts an element`() {
    val date = LocalDateTime.now()

    val queue = PriorityQueue<EmploymentRequest>()
    AddCommand(jsonDeserializer).run("{\"applicant\": \"joe\"," +
      "\"date\": \"$date\"}", queue)

    assertEquals(EmploymentRequest("joe", date), queue.peek())
  }
}