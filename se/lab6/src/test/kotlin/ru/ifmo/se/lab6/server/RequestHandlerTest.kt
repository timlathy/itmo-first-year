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
    val response = readResponseWithQueue("{invalid request", PriorityBlockingQueue())
    assertEquals("""{"status":400""" +
      ""","data":"Unable to parse the request; please make sure the data you are sending is valid JSON"}""", response)
  }

  @Test
  fun `responds with 422 for unknown commands`() {
    val response = readResponseWithQueue("""{"action":"die"}""", PriorityBlockingQueue())
    assertEquals("""{"status":422,"data":"Unknown command \"die\""}""", response)
  }

  @Test
  fun `responds with 422 for commands missing a payload`() {
    val response = readResponseWithQueue("""{"action":"add"}""", PriorityBlockingQueue())
    assertEquals("""{"status":422,"data":"Command \"add\" requires an argument specified in the \"payload\" field"}""", response)
  }

  @Test
  fun `responds with 422 for deserialization errors`() {
    val response = readResponseWithQueue("""{"action":"add","payload":{"status":"Processing"}}""", PriorityBlockingQueue())
    assertEquals("""{"status":422,"data":"Request payload is invalid: applicant name cannot be blank"}""", response)
  }

  @Test
  fun `responds with a list of commands for list_commands`() {
    val response = readResponseWithQueue("""{"action":"list_commands"}""", PriorityBlockingQueue())
    assertEquals("""{"status":200,"data":[{"name":"clear","argument":"none"},{"name":"add","argument":"json"},""" +
      """{"name":"add_if_max","argument":"json"},{"name":"add_if_min","argument":"json"},{"name":"remove_lower","argument":"json"},""" +
      """{"name":"remove_greater","argument":"json"},{"name":"remove_first","argument":"none"},{"name":"remove_last","argument":"none"},""" +
      """{"name":"remove","argument":"json"},{"name":"remove_all","argument":"json"},{"name":"info","argument":"none"}]}""", response)
  }

  /* End of tests, helper functions below. */

  private fun readResponseWithQueue(request: String, queue: PriorityBlockingQueue<EmploymentRequest>): String =
    readResponseFor(request) { socket ->
      RequestHandler(socket, CommandRunner(EmploymentRequestCommands.commandList, queue)).run()
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
