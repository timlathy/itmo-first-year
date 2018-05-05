package ru.ifmo.se.lab7.server

import com.fasterxml.jackson.databind.ObjectMapper
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import ru.ifmo.se.lab7.server.EmploymentRequestCommands.Companion.STATUS_ELEMENT_ADDED
import ru.ifmo.se.lab7.server.EmploymentRequestCommands.Companion.STATUS_MANY_REMOVED
import ru.ifmo.se.lab7.server.EmploymentRequestCommands.Companion.STATUS_ONE_REMOVED
import ru.ifmo.se.lab7.server.EmploymentRequestCommands.Companion.STATUS_UNCHANGED
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit.SECONDS

@ExtendWith(MockitoExtension::class)
class RequestHandlerCommandsTest {
  @Test
  fun `"clear" removes all elements`() = withinTestEnvironment {
    val ers = arrayOf(EmploymentRequest("joe"), EmploymentRequest("amy"))
    setQueue(*ers)
    assertTrue(queue.isNotEmpty())

    makeRequest("""{"action":"clear"}""")
    assertResponseEquals("""{"status":200,"data":"2 $STATUS_MANY_REMOVED"}""")
    assertChangesEqual(*allRemoved(ers))

    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"add" inserts an element`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)

    makeRequest("""{"action":"add","payload":{"applicant":"joe","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ELEMENT_ADDED: ${EmploymentRequest("joe", date)}"}""")
    assertChangesEqual(added(EmploymentRequest("joe", date)))

    assertEquals(EmploymentRequest("joe", date), queue.peek())
  }

  @Test
  fun `"add_if_max" inserts an element if it has the highest priority in the queue`() = withinTestEnvironment {
    var date = LocalDateTime.now().truncatedTo(SECONDS)
    val ers = arrayOf(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("mary", date))
    setQueue(*ers)

    makeRequest("""{"action":"add_if_max","payload":{"applicant":"joe","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""")
    assertChangesEqual()
    assertEquals(2, queue.size)

    date = date.minusHours(2).truncatedTo(SECONDS)

    makeRequest("""{"action":"add_if_max","payload":{"applicant":"joe","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ELEMENT_ADDED: ${EmploymentRequest("joe", date)}"}""")
    assertChangesEqual(added(EmploymentRequest("joe", date)))
    assertEquals(3, queue.size)
  }

  @Test
  fun `"add_if_min" inserts an element if it has the lowest priority in the queue`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val ers = arrayOf(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("mary", date.minusSeconds(1)))
    setQueue(*ers)

    makeRequest("""{"action":"add_if_min","payload":{"applicant":"joe","date":""" +
      """"${date.minusMinutes(1).truncatedTo(SECONDS)}"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""")
    assertChangesEqual()
    assertEquals(2, queue.size)

    makeRequest("""{"action":"add_if_min","payload":{"applicant":"joe","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ELEMENT_ADDED: ${EmploymentRequest("joe", date)}"}""")
    assertChangesEqual(added(EmploymentRequest("joe", date)))
    assertEquals(3, queue.size)
  }

  @Test
  fun `"remove_lower" removes all lower-priority elements`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val ers = arrayOf(
      EmploymentRequest("amy", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("jane", date),
      EmploymentRequest("steve", date, status = EmploymentRequest.Status.REJECTED))
    setQueue(*ers)

    makeRequest("""{"action":"remove_lower","payload":{"applicant":"-","date":"$date"}}""")

    assertResponseEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""")
    assertChangesEqual(
      removed(EmploymentRequest("steve", date, status = EmploymentRequest.Status.REJECTED)))
    assertQueueContentsEqual(
      *ers.filter { it.status != EmploymentRequest.Status.REJECTED }.toTypedArray())
  }

  @Test
  fun `"remove_greater" removes all higher-priority elements`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    setQueue(
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("jane", date),
      EmploymentRequest("amy", date.plusHours(1)))

    makeRequest("""{"action":"remove_greater","payload":{"applicant":"-","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"2 $STATUS_MANY_REMOVED"}""")
    assertChangesEqual(
      removed(EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED)),
      removed(EmploymentRequest("joe", date.minusHours(1))))
    assertQueueContentsEqual(
      EmploymentRequest("jane", date),
      EmploymentRequest("amy", date.plusHours(1)))
  }

  @Test
  fun `"remove_first" removes the head element`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    setQueue(
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("jane", date),
      EmploymentRequest("joe", date.minusHours(1)))

    makeRequest("""{"action":"remove_first"}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""")
    assertChangesEqual(removed(EmploymentRequest("mary", date.minusHours(2))))
    assertQueueContentsEqual(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("jane", date))

    setQueue()
    makeRequest("""{"action":"remove_first"}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""")
    assertChangesEqual()
  }

  @Test
  fun `"remove_last" removes the tail element`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    setQueue(
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("jane", date),
      EmploymentRequest("joe", date.minusHours(1)))

    makeRequest("""{"action":"remove_last"}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""")
    assertChangesEqual(removed(EmploymentRequest("jane", date)))
    assertQueueContentsEqual(
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("joe", date.minusHours(1)))

    /* Now including status... */

    setQueue(
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("joe", date.minusHours(1),
        status = EmploymentRequest.Status.REJECTED),
      EmploymentRequest("jane", date))

    makeRequest("""{"action":"remove_last"}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""")
    assertChangesEqual(removed(EmploymentRequest("joe", date.minusHours(1),
      status = EmploymentRequest.Status.REJECTED)))
    assertQueueContentsEqual(
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("jane", date))

    makeRequest("""{"action":"remove_last"}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""")
    assertChangesEqual(removed(EmploymentRequest("jane", date)))
    assertQueueContentsEqual(
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED)
    )
  }

  @Test
  fun `"remove" removes an element`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val element = EmploymentRequest("jane", date, status = EmploymentRequest.Status.REJECTED)
    setQueue(element)

    makeRequest("""{"action":"remove","payload":{"applicant":"jane","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_UNCHANGED"}""")
    assertChangesEqual()
    assertQueueContentsEqual(element)

    makeRequest("""{"action":"remove","payload":{"applicant":"jane","date":"$date","status":"Rejected"}}""")
    assertResponseEquals("""{"status":200,"data":"$STATUS_ONE_REMOVED"}""")
    assertChangesEqual(removed(element))
    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"remove_all" removes all equivalent elements`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val ers = arrayOf(
      EmploymentRequest("joe", date),
      EmploymentRequest("joe", date),
      EmploymentRequest("jane", date))
    setQueue(*ers)

    makeRequest("""{"action":"remove_all","payload":{"applicant":"joe","date":"$date"}}""")
    assertResponseEquals("""{"status":200,"data":"2 $STATUS_MANY_REMOVED"}""")
    assertChangesEqual(*allRemoved(ers.slice(0..1).toTypedArray()))
    assertQueueContentsEqual(EmploymentRequest("jane", date))
  }

  @Test
  fun `"dump_queue" returns all elements in the queue`() = withinTestEnvironment {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val ers = arrayOf(
      EmploymentRequest("bob", date.minusHours(1),
        status = EmploymentRequest.Status.REJECTED),
      EmploymentRequest("joe", date),
      EmploymentRequest("jane", date))
    setQueue(*ers)

    val expected = ObjectMapper()
      .apply { findAndRegisterModules() }
      .writeValueAsString(ers.sortedArrayWith(QUEUE_COMPARATOR))

    makeRequest("""{"action":"dump_queue"}""")
    assertResponseEquals("""{"status":200,"data":$expected}""")

    /* Should not modify the queue */
    assertChangesEqual()
    assertQueueContentsEqual(*ers.sortedArrayWith(QUEUE_COMPARATOR))
  }

  @Test
  fun `"argument_schema" returns a json schema of the accepted command argument`() = withinTestEnvironment {
    makeRequest("""{"action":"argument_schema"}""")
    assertResponseEquals("""{"status":200,"data":{"type":"object","id":"urn:jsonschema:ru:ifmo:se:lab7:server:EmploymentRequest",""" +
      """"properties":{"applicant":{"type":"string","required":true},"date":{"type":"string","required":true,"format":"date-time"},""" +
      """"interviewLocation":{"type":"object","id":"urn:jsonschema:kotlin:Pair<java:lang:Double,java:lang:Double>","properties":{"first":{"type":"number","required":true},"second":{"type":"number","required":true}}},""" +
      """"details":{"type":"string","required":true},"status":{"type":"string","required":true,"enum":["Interview scheduled","Processing","Rejected"]}}}}""")
    assertChangesEqual()
  }
}
