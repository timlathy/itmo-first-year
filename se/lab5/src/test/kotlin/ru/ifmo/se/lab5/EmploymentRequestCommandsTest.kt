package ru.ifmo.se.lab5

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.time.LocalDateTime
import java.util.*

import ru.ifmo.se.lab5.EmploymentRequestCommands.*

class EmploymentRequestCommandsTest {
  private val jsonDeserializer = Deserializer(EmploymentRequest::class.java)

  @Test
  fun `"clear" removes all elements`() {
    val queue = queue(EmploymentRequest("name", LocalDateTime.now()))
    assertTrue(queue.isNotEmpty())

    ClearCommand().run("", queue)
    assertTrue(queue.isEmpty())
  }

  @Test
  fun `"add" inserts an element`() {
    val date = LocalDateTime.now()

    val queue = queue()
    AddCommand(jsonDeserializer).run("{\"applicant\": \"joe\"," +
      "\"date\": \"$date\"}", queue)

    assertEquals(EmploymentRequest("joe", date), queue.peek())
  }

  @Test
  fun `"remove_lower" removes all lower-priority elements`() {
    val date = LocalDateTime.now()

    val queue = queue(
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("amy", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("steve", status = EmploymentRequest.Status.REJECTED),
      EmploymentRequest("jane", date)
    )

    RemoveLowerCommand(jsonDeserializer).run(
      "{\"applicant\": \"-\", \"date\": \"$date\"}", queue)

    assertQueueContentsEqual(queue,
      EmploymentRequest("amy", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("joe", date.minusHours(1)),
      EmploymentRequest("jane", date))
  }

  @Test
  fun `"remove_last" removes the tail element`() {
    val date = LocalDateTime.now()

    var queue = queue(
      EmploymentRequest("mary", date.minusHours(2)),
      EmploymentRequest("jane", date),
      EmploymentRequest("joe", date.minusHours(1))
    )

    RemoveLastCommand().run("", queue)

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

    RemoveLastCommand().run("", queue)

    assertQueueContentsEqual(queue,
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest("jane", date)
    )

    RemoveLastCommand().run("", queue)

    assertQueueContentsEqual(queue,
      EmploymentRequest("bob", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED)
    )
  }

  private fun queue(vararg elements: EmploymentRequest) =
    PriorityQueue<EmploymentRequest>(QUEUE_COMPARATOR).apply { addAll(listOf(*elements)) }

  private inline fun<reified E> assertQueueContentsEqual(queue: PriorityQueue<E>, vararg expected: E) {
    /* PriorityQueue#toArray() returns an _unsorted_ array, hence using #poll()
     * to retrieve elements in order. */
    val actual = mutableListOf<E>().apply {
      val source = PriorityQueue<E>(queue)
      while (source.isNotEmpty()) add(source.poll())
    }.toTypedArray()

    assertArrayEquals(expected, actual)
  }
}