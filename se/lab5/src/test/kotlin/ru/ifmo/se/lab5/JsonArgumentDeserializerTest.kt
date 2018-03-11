package ru.ifmo.se.lab5

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import java.time.LocalDateTime

class JsonArgumentDeserializerTest {
  private val jsonDeserializer = Deserializer(EmploymentRequest::class.java)

  @Test
  fun `reports errors for unknown properties`() {
    val exception = assertThrows<CommandRunner.CommandExecutionException> {
      jsonDeserializer.fromString("{\"a\": 5}")
    }
    Assertions.assertEquals("Unknown property \"a\", expecting one of " +
      "\"applicant\", \"date\", \"details\", \"status\"", exception.message)
  }

  @Test
  fun `reports errors for json parsing errors (missing tokens, etc)`() {
    val exception = assertThrows<CommandRunner.CommandExecutionException> {
      jsonDeserializer.fromString("{\"a\":")
    }
    Assertions.assertEquals("Unable to read the employment request specified; " +
      "please make sure the data you are entering is a valid JSON", exception.message)
  }

  @Test
  fun `reports errors for invalid enum values`() {
    val exception = assertThrows<CommandRunner.CommandExecutionException> {
      jsonDeserializer.fromString("{\"status\":\"h\"}")
    }
    Assertions.assertEquals("Unknown value \"h\" for an enumerated property \"status\", " +
        "expecting one of \"Interview scheduled\", \"Processing\", \"Rejected\"", exception.message)
  }

  @Test
  fun `reports bean validation errors`() {
    var exception = assertThrows<CommandRunner.CommandExecutionException> {
      jsonDeserializer.fromString("{}")
    }
    Assertions.assertEquals("The employment request specified is invalid: " +
      "applicant name cannot be blank", exception.message)

    exception = assertThrows<CommandRunner.CommandExecutionException> {
      val dateInFuture = LocalDateTime.now().plusMinutes(1)
      jsonDeserializer.fromString("{\"date\": \"$dateInFuture\"}")
    }
    Assertions.assertEquals("The employment request specified is invalid: " +
      "applicant name cannot be blank, request date cannot refer to the future", exception.message)
  }
}
