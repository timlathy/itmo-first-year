package org.pearl

import kotlin.test.Test
import kotlin.test.assertEquals

import TestModel
import org.pearl.query.from
import org.pearl.query.not
import java.time.LocalDateTime

class SelectQueryTest {
  data class Product(val id: Int = 0, val testId: Int = 0) : Model()

  @Test
  fun `should convert predicates to SQL, keeping bound variables`() {
    val date = LocalDateTime.now()
    val (sql, bindings) = from<TestModel>()
      .where { ((it["id"] lt 10) and (it["name"] eq "'quotes\"")) or (it["date"] notEq date) }.toParameterizedSql()

    assertEquals("""SELECT * FROM "TestModel" WHERE (("TestModel"."id" < ? AND "TestModel"."name" = ?) OR "TestModel"."date" != ?)""", sql)
    assertEquals(listOf(10, "'quotes\"", date), bindings)
  }

  @Test
  fun `should produce correct SQL for negated predicates`() {
    val (sql, bindings) = from<TestModel>()
      .where { not((it["id"] lt 10) and (it["name"] eq "fff") or not(it["id"] notEq 10)) }.toParameterizedSql()

    assertEquals("""SELECT * FROM "TestModel" WHERE NOT (("TestModel"."id" < ? AND "TestModel"."name" = ?) OR NOT "TestModel"."id" != ?)""", sql)
    assertEquals(listOf(10, "fff", 10), bindings)
  }

  @Test
  fun `should support IS NULL, IS NOT NULL matches`() {
    val (sql, bindings) = from<TestModel>()
      .where { it["name"].isNull() and it["enum"].isNotNull() }.toParameterizedSql()

    assertEquals("""SELECT * FROM "TestModel" WHERE ("TestModel"."name" IS NULL AND "TestModel"."enum" IS NOT NULL)""", sql)
    assertEquals(emptyList(), bindings)
  }

  @Test
  fun `should transform nested queries to valid SQL code`() {
    val date = LocalDateTime.now()
    val (sql, bindings) = from<TestModel>()
      .where { (it["date"] notEq date) and (it["id"] `in` from<Product>().where { it["id"] gt 7 }.select("testId")) and (it["name"] eq "f") }.toParameterizedSql()

    assertEquals(
      """SELECT * FROM "TestModel" WHERE (("TestModel"."date" != ? AND "TestModel"."id" IN (SELECT "Product"."testId" FROM "Product" WHERE "Product"."id" > ?)) AND "TestModel"."name" = ?)""",
      sql)
    assertEquals(listOf(date, 7, "f"), bindings)
  }

  @Test
  fun `should support matching on enums`() {
    val (sql, bindings) = from<TestModel>()
      .where { it["enum"] eq TestModel.TestEnum.T1 }.toParameterizedSql()

    assertEquals("""SELECT * FROM "TestModel" WHERE "TestModel"."enum" = ?""", sql)
    assertEquals(listOf(TestModel.TestEnum.T1), bindings)
  }
}
