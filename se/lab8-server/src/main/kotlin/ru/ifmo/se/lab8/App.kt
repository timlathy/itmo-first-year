package ru.ifmo.se.lab8

import spark.kotlin.*
import org.pearl.repo.Repo

fun main(args: Array<String>) {
  Repo.connect("localhost", 5432, dbname = "pearl", username = "pearl", password = "pearl")

  if (args.size == 1 && args[0] == "init") {
    println("""Creating "EmplomentRequest" table...""")
    Repo.createTable<EmploymentRequest>()
    return
  }

  val http = ignite {
      port = 8080
      staticFiles {
        location = "client/public"
      }
    }

  ApiController(http)
}

