package org.pearl

import kotlin.test.Test
import kotlin.test.assertEquals

import TestModel
import org.pearl.query.UpdateQuery
import org.pearl.query.from
import org.pearl.query.insert
import org.pearl.query.not
import java.time.LocalDateTime

class UpdateQueryTest {
  @Test
  fun `should build correct SQL for insertion`() {
    val date = LocalDateTime.of(2017, 2, 3, 17, 56, 20)
    val (sql, bindings) = UpdateQuery.InsertQuery(TestModel(name = "h", size = 30, date = date)).toParameterizedSql()

    assertEquals("""INSERT INTO "TestModel" ("date", "name", "size") VALUES (?, ?, ?) RETURNING "id"""", sql)
    assertEquals(listOf(date, "h", 30), bindings)
  }
}
