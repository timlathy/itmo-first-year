package ru.ifmo.se.lab6.client

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import java.net.InetSocketAddress

@ExtendWith(MockitoExtension::class)
class CommandRunnerTest {
  class MockServerConnection(private val expectedAction: String,
                             private val expectedResponse: String): ServerConnection(InetSocketAddress(8080)) {
    override fun fetchResponse(action: String, rawPayload: String?): String {
      assertEquals(expectedAction, action)
      return expectedResponse
    }
  }

  @Test
  fun `companion fetches a list of commands`() {
    val conn = MockServerConnection("list_commands",
      """[{"name":"clear","argument":"none"},{"name":"add","argument":"json"},""" +
          """{"name":"add_if_max","argument":"json"},{"name":"add_if_min","argument":"json"},{"name":"remove_lower","argument":"json"},""" +
          """{"name":"remove_greater","argument":"json"},{"name":"remove_first","argument":"none"},{"name":"remove_last","argument":"none"},""" +
          """{"name":"remove","argument":"json"},{"name":"remove_all","argument":"json"},{"name":"info","argument":"none"},""" +
          """{"name":"argument_schema","argument":"none"}]""")
      val commands = CommandRunner.fetchCommands(conn)
      assertArrayEquals(arrayOf(
        ServerCommand("clear", ServerCommand.ArgumentType.NONE),
        ServerCommand("add", ServerCommand.ArgumentType.JSON),
        ServerCommand("add_if_max", ServerCommand.ArgumentType.JSON),
        ServerCommand("add_if_min", ServerCommand.ArgumentType.JSON),
        ServerCommand("remove_lower", ServerCommand.ArgumentType.JSON),
        ServerCommand("remove_greater", ServerCommand.ArgumentType.JSON),
        ServerCommand("remove_first", ServerCommand.ArgumentType.NONE),
        ServerCommand("remove_last", ServerCommand.ArgumentType.NONE),
        ServerCommand("remove", ServerCommand.ArgumentType.JSON),
        ServerCommand("remove_all", ServerCommand.ArgumentType.JSON),
        ServerCommand("info", ServerCommand.ArgumentType.NONE),
        ServerCommand("argument_schema", ServerCommand.ArgumentType.NONE)), commands.toTypedArray())
  }

  @Test
  fun `companion fetches argument schema`() {
    val conn = MockServerConnection("argument_schema",
      """{"type":"object","id":"urn:jsonschema:ru:ifmo:se:lab6:server:EmploymentRequest",""" +
      """"properties":{"applicant":{"type":"string","required":true},"date":{"type":"string","required":true,"format":"date-time"},""" +
      """"interviewLocation":{"type":"object","id":"urn:jsonschema:kotlin:Pair<java:lang:Double,java:lang:Double>","properties":{"first":{"type":"number","required":true},"second":{"type":"number","required":true}}},"details":{"type":"string","required":true},"status":{"type":"string","required":true,"enum":["Interview scheduled","Processing","Rejected"]}}}""")
    val schema = CommandRunner.fetchSchema(conn)
    assertTrue(schema.properties["applicant"]?.required ?: false)
  }
}
