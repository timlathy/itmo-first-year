package ru.ifmo.se.lab5

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows

import ru.ifmo.se.lab5.EmploymentRequestCommands.*
import java.time.LocalDateTime
import java.util.*

class EmploymentRequestCommandsTest {
  @Test
  fun `"clear" removes all elements`() {
    val queue = PriorityQueue(listOf(EmploymentRequest("name", LocalDateTime.now())))
    assertTrue(queue.isNotEmpty())

    ClearCommand().run("", queue)
    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"add" reports deserialization errors`() {
    var exception = assertThrows<CommandRunner.CommandExecutionException> {
      AddCommand().run("{\"a\": 5}", PriorityQueue())
    }
    assertEquals("Unknown field \"a\". " +
      "Valid fields for employment requests are " +
      "\"applicant\", \"date\", \"details\", \"status\"", exception.message)

    exception = assertThrows<CommandRunner.CommandExecutionException> {
      AddCommand().run("{\"a\":", PriorityQueue())
    }
    assertEquals("Unable to read the employment request specified; " +
      "please make sure the data you are entering is a valid JSON", exception.message)

    exception = assertThrows<CommandRunner.CommandExecutionException> {
      AddCommand().run("{}", PriorityQueue())
    }
    assertEquals("The employment request specified is invalid: " +
      "applicant name cannot be blank", exception.message)

    exception = assertThrows<CommandRunner.CommandExecutionException> {
      val dateInFuture = LocalDateTime.now().plusMinutes(1)
      AddCommand().run("{\"date\": \"$dateInFuture\"}", PriorityQueue())
    }
    assertEquals("The employment request specified is invalid: " +
      "applicant name cannot be blank, request date cannot refer to the future", exception.message)
  }

  @Test
  fun `"add" inserts an element`() {
    val date = LocalDateTime.now()

    val queue = PriorityQueue<EmploymentRequest>()
    AddCommand().run("{\"applicant\": \"joe\"," +
      "\"date\": \"$date\"}", queue)

    assertEquals(EmploymentRequest("joe", date), queue.peek())
  }
}