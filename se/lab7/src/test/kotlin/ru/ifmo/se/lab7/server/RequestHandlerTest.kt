package ru.ifmo.se.lab7.server

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.api.Assertions.assertEquals

@ExtendWith(MockitoExtension::class)
class RequestHandlerTest {
  @Test
  fun `responds with 400 for invalid requests`() = withinTestEnvironment {
    makeRequest("{invalid request")
    assertResponseEquals("""{"status":400""" +
      ""","data":"Unable to parse the request; please make sure the data you are sending is valid JSON"}""")
  }

  @Test
  fun `responds with 422 for unknown commands`() = withinTestEnvironment {
    makeRequest("""{"action":"die"}""")
    assertResponseEquals("""{"status":422,"data":"Unknown command \"die\""}""" )
  }

  @Test
  fun `responds with 422 for commands missing a payload`() = withinTestEnvironment {
    makeRequest("""{"action":"add"}""")
    assertResponseEquals("""{"status":422,"data":"Command \"add\" requires an argument specified in the \"payload\" field"}""" )
  }

  @Test
  fun `responds with 422 for deserialization errors`() = withinTestEnvironment {
    makeRequest("""{"action":"add","payload":{"status":"Processing"}}""")
    assertResponseEquals("""{"status":422,"data":"Request payload is invalid: applicant name cannot be blank"}""" )
  }

  @Test
  fun `responds with a list of commands for list_commands`() = withinTestEnvironment {
    makeRequest("""{"action":"list_commands"}""")
    assertResponseEquals("""{"status":200,"data":[{"name":"clear","argument":"none"},{"name":"add","argument":"json"},""" +
      """{"name":"add_if_max","argument":"json"},{"name":"add_if_min","argument":"json"},{"name":"remove_lower","argument":"json"},""" +
      """{"name":"remove_greater","argument":"json"},{"name":"remove_first","argument":"none"},{"name":"remove_last","argument":"none"},""" +
      """{"name":"remove","argument":"json"},{"name":"remove_all","argument":"json"},{"name":"info","argument":"none"},""" +
      """{"name":"dump_queue","argument":"none"},{"name":"argument_schema","argument":"none"}]}""" )
  }
}
