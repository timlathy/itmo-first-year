package org.pearl

import kotlin.test.Test

import TestModel
import org.pearl.repo.Repo

class RepoTest {
  @Test
  fun `should generate table name from class name`() {
    Repo.connectToUrl("localhost", 5432, dbname = "pearl", username = "pearl", password = "pearl")

    //val h = Repo.fetchMany(from<TestModel>())

    //Repo.createTable<TestModel>()
  }
}
