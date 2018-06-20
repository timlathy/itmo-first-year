package org.pearl

import kotlin.test.Test
import kotlin.test.assertEquals

import TestModel
import org.pearl.query.from
import org.pearl.query.not

class QueryTest {
  class Product(val id: Int = 0, val testId: Int = 0): Model()

  @Test
  fun `should convert predicates to SQL code`() {
    assertEquals(
      """SELECT * FROM "TestModel" WHERE (("TestModel"."id" < 10 AND "TestModel"."name" = '\'quotes"') OR "TestModel"."id" != 10)""",
      from<TestModel>().where { ((it["id"] lt 10) and (it["name"] eq "'quotes\"")) or (it["id"] notEq 10) }.toSql())

    assertEquals(
      """SELECT * FROM "TestModel" WHERE NOT (("TestModel"."id" < 10 AND "TestModel"."name" = '\'quotes"') OR NOT "TestModel"."id" != 10)""",
      from<TestModel>().where { not((it["id"] lt 10) and (it["name"] eq "'quotes\"") or not(it["id"] notEq 10)) }.toSql())
  }

  @Test
  fun `should transform nested queries to valid SQL code`() {
    assertEquals(
      """SELECT * FROM "TestModel" WHERE "TestModel"."id" IN (SELECT "Product"."testId" FROM "Product")""",
      from<TestModel>().where { it["id"] `in` from<Product>().select("testId") }.toSql())
  }
}
