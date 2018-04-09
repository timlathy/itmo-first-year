package ru.ifmo.se.lab6.server

import com.fasterxml.jackson.databind.ObjectMapper
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import ru.ifmo.se.lab6.server.EmploymentRequestCommands.Companion.STATUS_CLEARED
import ru.ifmo.se.lab6.server.EmploymentRequestCommands.Companion.STATUS_ELEMENT_ADDED
import ru.ifmo.se.lab6.server.EmploymentRequestCommands.Companion.STATUS_MANY_REMOVED
import ru.ifmo.se.lab6.server.EmploymentRequestCommands.Companion.STATUS_ONE_REMOVED
import ru.ifmo.se.lab6.server.EmploymentRequestCommands.Companion.STATUS_UNCHANGED
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit.SECONDS

@ExtendWith(MockitoExtension::class)
class RequestHandlerCommandsTest {
  @Test
  fun `"clear" removes all elements`() {
    val queue = queue(EmploymentRequest("joe"), EmploymentRequest("amy"))
    assertTrue(queue.isNotEmpty())

    assertEquals("""{"status":200,"data":"$STATUS_CLEARED"}""",
      readResponseWithQueue("""{"action":"clear"}""", queue))

    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"add" inserts an element`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue()

    assertEquals("""{"status":200,"data":"$STATUS_ELEMENT_ADDED: ${EmploymentRequest("joe", date)}"}""",
      readResponseWithQueue(
        """{"action":"add","payload":{"applicant":"joe","date":"$date"}}""", queue))

    assertEquals(EmploymentRequest("joe", date), queue.peek())
  }

  @Test
  fun `"add_if_max" inserts an element if it has the highest priority in the queue`() {
    var date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("mary", date))

    assertEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""",
      readResponseWithQueue(
        """{"action":"add_if_max","payload":{"applicant":"joe","date":"$date"}}""", queue))
    assertEquals(2, queue.size)

    date = date.minusHours(2).truncatedTo(SECONDS)
    assertEquals("""{"status":200,"data":"$STATUS_ELEMENT_ADDED: ${EmploymentRequest("joe", date)}"}""",
      readResponseWithQueue(
        """{"action":"add_if_max","payload":{"applicant":"joe","date":"$date"}}""", queue))
    assertEquals(3, queue.size)
  }

  @Test
  fun `"add_if_min" inserts an element if it has the lowest priority in the queue`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("mary", date.minusSeconds(1)))

    assertEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""",
      readResponseWithQueue(
        """{"action":"add_if_min","payload":{"applicant":"joe","date":""" +
          """"${date.minusMinutes(1).truncatedTo(SECONDS)}"}}""", queue))
    assertEquals(2, queue.size)

    assertEquals("""{"status":200,"data":"$STATUS_ELEMENT_ADDED: ${EmploymentRequest("joe", date)}"}""",
      readResponseWithQueue(
        """{"action":"add_if_min","payload":{"applicant":"joe","date":"$date"}}""", queue))
    assertEquals(3, queue.size)
  }

  @Test
  fun `"remove_lower" removes all lower-priority elements`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("amy", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("steve", status = EmploymentRequest.Status.REJECTED),
      EmploymentRequest("jane", date)
    )

    assertEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""",
      readResponseWithQueue(
        """{"action":"remove_lower","payload":{"applicant":"-","date":"$date"}}""", queue))
    assertQueueContentsEqual(queue,
      EmploymentRequest("amy", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("jane", date))
  }

  @Test
  fun `"remove_greater" removes all higher-priority elements`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("amy", date.plusHours(1)),
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("jane", date)
    )

    assertEquals("""{"status":200,"data":"2 $STATUS_MANY_REMOVED"}""",
      readResponseWithQueue(
        """{"action":"remove_greater","payload":{"applicant":"-","date":"$date"}}""", queue))

    assertQueueContentsEqual(queue,
      EmploymentRequest("jane", date),
      EmploymentRequest("amy", date.plusHours(1)))
  }

  @Test
  fun `"remove_first" removes the head element`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("jane", date),
      EmploymentRequest("joe", date.minusHours(1))
    )

    assertEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""",
      readResponseWithQueue("""{"action":"remove_first"}""", queue))

    assertQueueContentsEqual(queue,
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("jane", date)
    )

    assertEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""",
      readResponseWithQueue("""{"action":"remove_first"}""", queue()))
  }

  @Test
  fun `"remove_last" removes the tail element`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    var queue = queue(
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("jane", date),
      EmploymentRequest("joe", date.minusHours(1))
    )

    assertEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""",
      readResponseWithQueue("""{"action":"remove_last"}""", queue))
    assertQueueContentsEqual(queue,
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("joe", date.minusHours(1))
    )

    /* Now including status... */

    queue = queue(
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("joe", date.minusHours(1),
        status = EmploymentRequest.Status.REJECTED),
      EmploymentRequest("jane", date)
    )

    assertEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""",
      readResponseWithQueue("""{"action":"remove_last"}""", queue))
    assertQueueContentsEqual(queue,
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("jane", date)
    )

    assertEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""",
      readResponseWithQueue("""{"action":"remove_last"}""", queue))
    assertQueueContentsEqual(queue,
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED)
    )
  }

  @Test
  fun `"remove" removes an element`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val element = EmploymentRequest("jane", date, status = EmploymentRequest.Status.REJECTED)
    val queue = queue(element)

    assertEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""",
      readResponseWithQueue(
        """{"action":"remove","payload":{"applicant":"jane","date":"$date"}}""", queue))
    assertQueueContentsEqual(queue, element)

    assertEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""",
      readResponseWithQueue(
        """{"action":"remove","payload":{"applicant":"jane","date":"$date","status":"Rejected"}}""", queue))
    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"remove_all" removes all equivalent elements`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("joe", date),
      EmploymentRequest("joe", date),
      EmploymentRequest("jane", date))

    assertEquals("""{"status":200,"data":"2 $STATUS_MANY_REMOVED"}""",
      readResponseWithQueue(
        """{"action":"remove_all","payload":{"applicant":"joe","date":"$date"}}""", queue))
    assertQueueContentsEqual(queue, EmploymentRequest("jane", date))
  }

  @Test
  fun `"dump_queue" returns all elements in the queue`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val queue = queue(
      EmploymentRequest("joe", date),
      EmploymentRequest("jane", date))

    val expected = ObjectMapper().apply { findAndRegisterModules() }.writeValueAsString(arrayOf(
      EmploymentRequest("joe", date),
      EmploymentRequest("jane", date)))

    assertEquals("""{"status":200,"data":$expected}""",
      readResponseWithQueue(
        """{"action":"dump_queue"}""", queue))

    /* Should not modify the queue */
    assertQueueContentsEqual(queue,
      EmploymentRequest("joe", date),
      EmploymentRequest("jane", date))
  }

  @Test
  fun `"argument_schema" returns a json schema of the accepted command argument`() {
    val response = readResponseWithQueue("""{"action":"argument_schema"}""", queue())
    assertEquals("""{"status":200,"data":{"type":"object","id":"urn:jsonschema:ru:ifmo:se:lab6:server:EmploymentRequest",""" +
      """"properties":{"applicant":{"type":"string","required":true},"date":{"type":"string","required":true,"format":"date-time"},""" +
      """"interviewLocation":{"type":"object","id":"urn:jsonschema:kotlin:Pair<java:lang:Double,java:lang:Double>","properties":{"first":{"type":"number","required":true},"second":{"type":"number","required":true}}},"details":{"type":"string","required":true},"status":{"type":"string","required":true,"enum":["Interview scheduled","Processing","Rejected"]}}}}""", response)
  }
}
