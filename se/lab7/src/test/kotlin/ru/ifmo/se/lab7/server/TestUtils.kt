package ru.ifmo.se.lab7.server

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.mockito.Mockito
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.net.Socket
import java.util.*
import java.util.concurrent.PriorityBlockingQueue

typealias QueueChange = CollectionChange<EmploymentRequest>

class CommandTestEnvironment {
  private var lastResponse: String = ""
  private var lastChanges: List<QueueChange> = emptyList()

  var queue: PriorityBlockingQueue<EmploymentRequest> = PriorityBlockingQueue(16, QUEUE_COMPARATOR)

  fun setQueue(vararg elements: EmploymentRequest) {
    queue = PriorityBlockingQueue(16, QUEUE_COMPARATOR).apply { addAll(listOf(*elements)) }
  }

  fun assertResponseEquals(expected: String) =
    assertEquals(expected + '\n', lastResponse)

  fun assertChangesEqual(vararg expected: QueueChange) {
    assertEquals(expected.size, lastChanges.size)
    assertArrayEquals(expected, lastChanges.toTypedArray())
  }

  fun assertQueueContentsEqual(vararg expected: EmploymentRequest) {
    val actual = mutableListOf<EmploymentRequest>().apply {
      /* Make a copy of the queue so as to not modify it */
      val sourceCopy = PriorityBlockingQueue(16, QUEUE_COMPARATOR).apply { addAll(queue) }

      /* PriorityBlockingQueue#toArray() returns an _unsorted_ array, hence using #poll()
       * to retrieve elements in order. */
      while (sourceCopy.isNotEmpty()) add(sourceCopy.poll())
    }.toTypedArray()

    assertArrayEquals(expected, actual)
  }

  fun makeRequest(request: String) {
    CommandRunner(EmploymentRequestCommands.commandList, queue).let { runner ->
      lastChanges = emptyList()
      runner.addCommandListener { command, changes: List<QueueChange> -> lastChanges = changes }

      lastResponse = readResponseFor(request) { socket -> RequestHandler(socket, runner).run() }
    }
  }
}

inline fun withinTestEnvironment(block: CommandTestEnvironment.() -> Unit): Unit =
  CommandTestEnvironment().run(block)

fun allAdded(elements: Array<EmploymentRequest>) = elements.map(::added).toTypedArray()

fun allRemoved(elements: Array<EmploymentRequest>) = elements.map(::removed).toTypedArray()

fun added(element: EmploymentRequest) = QueueChange(element, CollectionChange.ChangeType.ADDITION)

fun removed(element: EmploymentRequest) = QueueChange(element, CollectionChange.ChangeType.REMOVAL)

inline fun readResponseFor(request: String, block: (Socket) -> Unit): String =
  Mockito.mock(Socket::class.java).let { socket ->
    val socketOutput = ByteArrayOutputStream()
    val socketInput = ByteArrayInputStream(request.toByteArray())

    Mockito.`when`(socket.getInputStream()).thenReturn(socketInput)
    Mockito.`when`(socket.getOutputStream()).thenReturn(socketOutput)
    Mockito.`when`(socket.isClosed).thenReturn(false)

    block(socket)
    socketOutput.toString()
  }
