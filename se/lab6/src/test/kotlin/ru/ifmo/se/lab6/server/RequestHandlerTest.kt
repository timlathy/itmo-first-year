package ru.ifmo.se.lab6.server

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.Mockito
import org.junit.jupiter.api.Assertions.assertEquals
import java.net.Socket
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.util.concurrent.PriorityBlockingQueue

@ExtendWith(MockitoExtension::class)
class RequestHandlerTest {
  @Test
  fun `responds with 400 for invalid requests`() {
    val response = readResponseFor("{invalid request") { socket ->
      val handler = RequestHandler(socket, CommandRunner(EmploymentRequestCommands.commandList, PriorityBlockingQueue()))
      handler.run()
    }

    assertEquals("{\"status\":400," +
      "\"data\":\"Unable to parse the request; please make sure the data you are sending is valid JSON\"}", response)
  }

  @Test
  fun `responds with a list of commands for list_commands`() {
    val response = readResponseFor("{\"action\":\"list_commands\"}") { socket ->
      val handler = RequestHandler(socket, CommandRunner(EmploymentRequestCommands.commandList, PriorityBlockingQueue()))
      handler.run()
    }

    assertEquals("{\"status\":200,\"data\":\"[{\\\"name\\\":\\\"clear\\\",\\\"argument\\\":\\\"none\\\"}," +
      "{\\\"name\\\":\\\"add\\\",\\\"argument\\\":\\\"json\\\"},{\\\"name\\\":\\\"add_if_max\\\",\\\"argument\\\":\\\"json\\\"}," +
      "{\\\"name\\\":\\\"add_if_min\\\",\\\"argument\\\":\\\"json\\\"},{\\\"name\\\":\\\"remove_lower\\\",\\\"argument\\\":\\\"json\\\"}," +
      "{\\\"name\\\":\\\"remove_greater\\\",\\\"argument\\\":\\\"json\\\"},{\\\"name\\\":\\\"remove_first\\\",\\\"argument\\\":\\\"none\\\"}," +
      "{\\\"name\\\":\\\"remove_last\\\",\\\"argument\\\":\\\"none\\\"},{\\\"name\\\":\\\"remove\\\",\\\"argument\\\":\\\"json\\\"}," +
      "{\\\"name\\\":\\\"remove_all\\\",\\\"argument\\\":\\\"json\\\"},{\\\"name\\\":\\\"info\\\",\\\"argument\\\":\\\"none\\\"}]\"}", response)
  }

  private fun readResponseFor(request: String, block: (Socket) -> Unit): String =
    Mockito.mock(Socket::class.java).let { socket ->
      val socketOutput = ByteArrayOutputStream()
      val socketInput = ByteArrayInputStream(request.toByteArray())

      Mockito.`when`(socket.getInputStream()).thenReturn(socketInput)
      Mockito.`when`(socket.getOutputStream()).thenReturn(socketOutput)
      Mockito.`when`(socket.isClosed).thenReturn(false)

      block(socket)
      socketOutput.toString()
    }
}
