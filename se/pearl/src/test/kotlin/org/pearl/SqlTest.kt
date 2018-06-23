package org.pearl

import Consts.defaultDate
import Consts.defaultZonedDate
import kotlin.test.Test
import kotlin.test.assertEquals

import java.time.LocalDateTime
import java.time.ZonedDateTime

import org.pearl.SqlTest.SqlTestModel.SampleEnum.*

class SqlTest {
  data class SqlTestModel(
    @Id val id: Int = 0,
    val name: String = "",
    val double: Double = 1.1,
    val date: LocalDateTime = defaultDate,
    val zonedDate: ZonedDateTime = defaultZonedDate,
    val enum: SampleEnum = SampleEnum.VAL1
  ): Model() {
    enum class SampleEnum { VAL1, VAL2 }
  }

  @Test
  fun `should generate table definitions`() {
    assertEquals("""CREATE TABLE "SqlTestModel"
      | ("date" timestamp NOT NULL,
      | "double" double precision NOT NULL,
      | "enum" text NOT NULL,
      | "id" serial PRIMARY KEY,
      | "name" text NOT NULL,
      | "zonedDate" timestampz NOT NULL)""".trimMargin().replace("\n", ""),
      Sql.tableDefinition(SqlTestModel()))
  }

  @Test
  fun `should generate parameterized INSERTs from changesets`() {
    val changeset = Changeset.newRecord<SqlTestModel>(
      params = mapOf("double" to "1.0", "enum" to "VAL2", "name" to "hey"),
      allowedParams = listOf("double", "enum", "name")
    )
    val (sql, bindings) = Sql.insert(changeset)

    assertEquals("""INSERT INTO "SqlTestModel" ("date", "double", "enum", "name", "zonedDate") VALUES (?, ?, ?, ?, ?) RETURNING *""", sql)
    assertEquals(listOf(defaultDate, 1.0, SqlTestModel.SampleEnum.VAL2, "hey", defaultZonedDate), bindings)
  }

  @Test
  fun `should generate parameterized UPDATEs from changesets`() {
    val changeset = Changeset.update(SqlTestModel(id = 2), mapOf("name" to "hhhhh", "enum" to "VAL2"), listOf("name", "enum"))
    val (sql, bindings) = Sql.update(changeset)

    assertEquals("""UPDATE "SqlTestModel" SET "enum" = ?, "name" = ? WHERE "id" = ? RETURNING *""", sql)
    assertEquals(listOf(VAL2, "hhhhh", 2), bindings)
  }
}
