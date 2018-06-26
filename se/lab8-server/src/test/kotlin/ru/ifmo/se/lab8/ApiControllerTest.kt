package ru.ifmo.se.lab8

import com.despegar.http.client.WithBodyMethod
import com.despegar.sparkjava.test.SparkServer
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.BeforeClass
import org.junit.ClassRule
import org.junit.Test
import org.pearl.repo.Repo
import spark.Service
import spark.Spark
import spark.kotlin.Http
import spark.servlet.SparkApplication
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit.SECONDS

class ApiControllerTest {
  class TestApp: SparkApplication {
    override fun init() {
      Auth.disableAuthTestingOnly()
      Spark::class.java
        .getDeclaredMethod("getInstance")
        .apply { isAccessible = true }
        .invoke(null)
        .let { service -> Http(service as Service) }
        .run { ApiController(this) }
    }
  }

  companion object {
    @ClassRule @JvmField var testServer: SparkServer<TestApp> = SparkServer(TestApp::class.java, 4567)

    @BeforeClass @JvmStatic fun init() {
      Repo.connect("localhost", 5432, dbname = "pearl", username = "pearl", password = "pearl")
    }
  }

  @Before
  fun reset() {
    Repo.rawSqlUpdate("""DROP TABLE IF EXISTS "EmploymentRequest"""")
    Repo.createTable<EmploymentRequest>()
  }

  fun toJson(obj: Any) = ObjectMapper().findAndRegisterModules().writeValueAsString(obj)

  fun get(path: String, handler: (Int, String) -> Any = { s, _ -> }) =
    testServer.get(path, false).let(testServer::execute).let { handler(it.code(), String(it.body())) }

  fun post(path: String, body: String, handler: (Int, String) -> Any = { s, _ -> }) =
    testServer.post(path, body, false).let(testServer::execute).let { handler(it.code(), String(it.body())) }

  fun delete(path: String, body: String = "", handler: (Int) -> Any = { assertEquals(200, it) }) =
    (object : WithBodyMethod("http://localhost:4567/$path", "DELETE", body, false) {}).let(testServer::execute).let { handler(it.code()) }

  fun fetchQueue(): List<EmReq> = get("/queue") { s, b ->
    assertEquals(200, s)
    ObjectMapper().findAndRegisterModules().readValue<List<EmReq>>(b)
  } as List<EmReq>

  @Test
  fun `POST queue creates new employment requests`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)

    post("/queue", """{"applicant": "h", "date": "$date", "locLatitude": 1.2, "locLongitude": 2.1,""" +
         """"details": "hh", "status": "REJECTED", "colorCode": "BLUE"}""") { status, body ->
      assertEquals(201, status)
      assertEquals("""{"id":1,"applicant":"h","date":"$date","locLatitude":1.2,"locLongitude":2.1,""" +
        """"details":"hh","status":"REJECTED","colorCode":"BLUE"}""",  body)
    }

    get("/queue") { status, body ->
      assertEquals(200, status)
      assertEquals("""[{"id":1,"applicant":"h","date":"$date","locLatitude":1.2,"locLongitude":2.1,""" +
        """"details":"hh","status":"REJECTED","colorCode":"BLUE"}]""",  body)
    }
  }

  @Test
  fun `POST queue?mode=if_max creates a request only if it has the highest priority`() {
    var date = LocalDateTime.now().truncatedTo(SECONDS)
    val initial = arrayOf(
      EmploymentRequest(id = 1, applicant = "joe", date = date.minusHours(1)),
      EmploymentRequest(id = 2, applicant = "mary", date = date)
    )
    initial.forEach { post("/queue", toJson(it)) }

    post("/queue?mode=if_max", """{"applicant":"joe","date":"$date"}""") { status, body ->
      assertEquals(200, status)
      assertEquals("", body)
    }

    assertEquals(2, fetchQueue().size)

    date = date.minusHours(2).truncatedTo(SECONDS)

    post("/queue?mode=if_max", """{"applicant":"joe","date":"$date"}""") { status, body ->
      assertEquals(201, status)
      assertEquals(toJson(EmploymentRequest(id = 3, applicant = "joe", date = date)), body)
    }

    assertEquals((initial + EmploymentRequest(id = 3, applicant = "joe", date = date)).toList(), fetchQueue())
  }

  @Test
  fun `POST queue?mode=if_min creates a request only if it has the lowest priority`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val initial = arrayOf(
      EmploymentRequest(id = 1, applicant = "joe", date = date.minusHours(1)),
      EmploymentRequest(id = 2, applicant = "mary", date = date.minusSeconds(1))
    )
    initial.forEach { post("/queue", toJson(it)) }

    post("/queue?mode=if_min", """{"applicant":"joe","date":"${date.minusMinutes(1).truncatedTo(SECONDS)}"}""") { status, body ->
      assertEquals(200, status)
      assertEquals("", body)
    }

    assertEquals(2, fetchQueue().size)

    post("/queue?mode=if_min", """{"applicant":"joe","date":"$date"}""") { status, body ->
      assertEquals(201, status)
      assertEquals(toJson(EmploymentRequest(id = 3, applicant = "joe", date = date)), body)
    }

    assertEquals((initial + EmploymentRequest(id = 3, applicant = "joe", date = date)).toList(), fetchQueue())
  }

  @Test
  fun `DELETE queue?mode=lesser removes all lower-priority elements`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val initial = arrayOf(
      EmploymentRequest(1, "amy", date.plusHours(1),
        status = EmploymentRequest.Status.INTERVIEW_SCHEDULED),
      EmploymentRequest(2, "mary", date.minusHours(2)),
      EmploymentRequest(3, "joe", date),
      EmploymentRequest(4, "jane", date))
    initial.forEach { post("/queue", toJson(it)) }

    /* mode=lesser requires an element to compare against */
    delete("/queue?mode=lesser") { status ->
      assertEquals(422, status)
    }

    delete("/queue?mode=lesser", """{"applicant":"-","date":"$date"}""")

    assertEquals(initial.filter { it.date < date }, fetchQueue())
  }

  @Test
  fun `DELETE queue?mode=greater removes all higher-priority elements`() {
    val date = LocalDateTime.now().truncatedTo(SECONDS)
    val initial = arrayOf(
      EmploymentRequest(1, "bob", date),
      EmploymentRequest(2, "joe", date.minusHours(1)),
      EmploymentRequest(3, "jane", date.plusSeconds(1)),
      EmploymentRequest(4, "amy", date.plusHours(1)))
    initial.forEach { post("/queue", toJson(it)) }

    delete("/queue?mode=greater", """{"applicant":"-","date":"$date"}""")

    assertEquals(initial.sliceArray(2..3).toList(), fetchQueue())
  }
}
