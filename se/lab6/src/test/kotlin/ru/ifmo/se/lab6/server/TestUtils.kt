package ru.ifmo.se.lab6.server

import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.mockito.Mockito
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.net.Socket
import java.util.concurrent.PriorityBlockingQueue

inline fun readResponseWithQueue(request: String, queue: PriorityBlockingQueue<EmploymentRequest>): String =
  readResponseFor(request) { socket ->
    RequestHandler(socket, CommandRunner(EmploymentRequestCommands.commandList, queue)).run()
  }

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

inline fun queue(vararg elements: EmploymentRequest): PriorityBlockingQueue<EmploymentRequest> =
  PriorityBlockingQueue(16, QUEUE_COMPARATOR).apply { addAll(listOf(*elements)) }

inline fun assertQueueContentsEqual(queue: PriorityBlockingQueue<EmploymentRequest>,
                                    vararg expected: EmploymentRequest) {
  /* PriorityQueue#toArray() returns an _unsorted_ array, hence using #poll()
   * to retrieve elements in order. */
  val actual = mutableListOf<EmploymentRequest>().apply {
    val source = PriorityBlockingQueue(16, QUEUE_COMPARATOR)

    source.addAll(queue)

    while (source.isNotEmpty()) add(source.poll())
  }.toTypedArray()

  assertArrayEquals(expected, actual)
}
