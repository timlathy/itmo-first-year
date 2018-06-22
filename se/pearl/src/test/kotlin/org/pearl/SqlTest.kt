package org.pearl

import kotlin.test.Test
import kotlin.test.assertEquals

import java.time.LocalDateTime
import java.time.ZonedDateTime

class SqlTest {
  class DDLTestModel(
    @Id val id: Int = 0,
    val name: String = "",
    val double: Double = 1.1,
    val date: LocalDateTime = LocalDateTime.now(),
    val zonedDate: ZonedDateTime = ZonedDateTime.now(),
    val enum: SampleEnum = SampleEnum.VAL1
  ): Model() {
    enum class SampleEnum { VAL1, VAL2 }
  }

  @Test
  fun `it should generate table definitions`() {
    assertEquals("""CREATE TABLE "DDLTestModel"
      | ("date" timestamp NOT NULL,
      | "double" double precision NOT NULL,
      | "enum" text NOT NULL,
      | "id" serial PRIMARY KEY,
      | "name" text NOT NULL,
      | "zonedDate" timestampz NOT NULL)""".trimMargin().replace("\n", ""),
      Sql.tableDefinition(DDLTestModel()))
  }
}
