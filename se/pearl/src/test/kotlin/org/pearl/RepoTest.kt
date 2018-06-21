package org.pearl

import kotlin.test.Test

import TestModel
import org.pearl.query.from
import org.pearl.query.insert
import org.pearl.repo.Repo
import java.time.LocalDateTime
import kotlin.test.BeforeTest
import kotlin.test.assertEquals

class RepoTest {
  @BeforeTest
  fun init() {
    Repo.connectToUrl("localhost", 5432, dbname = "pearl", username = "pearl", password = "pearl")

    Repo.rawSqlUpdate("""DROP TABLE IF EXISTS "TestModel"""")
  }

  @Test
  fun `should create tables`() {
    Repo.createTable<TestModel>()
    assertEquals(emptyList(), Repo.fetchMany(from<TestModel>()))
  }

  @Test
  fun `should insert new records`() {
    try { Repo.createTable<TestModel>() } catch (e: Exception) { }
    insert(TestModel(name = "aaa", date = LocalDateTime.of(2017, 2, 3, 10, 0, 0), size = 80))
    insert(TestModel(name = "bbb", size = 20))

    assertEquals(listOf(1, 2), Repo.fetchMany(from<TestModel>().where { it["size"] lt 200 }).map { it.id })
    assertEquals(listOf(1), Repo.fetchMany(from<TestModel>().where { it["name"] eq "aaa" }).map { it.id })
  }
}
