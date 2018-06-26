package ru.ifmo.se.lab8

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import org.pearl.Changeset
import org.pearl.Changeset.Companion.newRecord
import org.pearl.query.*
import org.pearl.repo.Repo
import ru.ifmo.se.lab8.EmploymentRequest.Companion.ALLOWED_PARAMS
import ru.ifmo.se.lab8.EmploymentRequest.Companion.ascOrder
import ru.ifmo.se.lab8.EmploymentRequest.Companion.descOrder
import ru.ifmo.se.lab8.EmploymentRequest.Companion.validate
import spark.kotlin.*

class ApiController(http: Http) {
  private val jsonMapper = ObjectMapper().findAndRegisterModules()

  val json = jsonMapper::writeValueAsString

  val readJsonMap = { body: String -> jsonMapper.readValue<Map<String, String>>(body) }

  init {
    http.before {
      if (!Auth.authenticateHeader(request.headers("Authorization"))) halt(403)
      println("Incoming connection ${request.requestMethod()} ${request.pathInfo()}")
    }

    http.get("/queue") { 
      Repo.many(from<EmReq>()).let(json)
    }

    http.post("/queue") { 
      val changeset = request.body()
        .let(readJsonMap)
        .let { newRecord<EmReq>(it, ALLOWED_PARAMS) }
        .let(::validate)

      if (changeset.errors.any()) {
        status(422)
        json(changeset.errors)
      }
      else {
        val result = Repo.one(when (request.queryParamOrDefault("mode", "uncond")) {
          "if_min" -> insert(changeset).where { EmReq.isMin(changeset) }
          "if_max" -> insert(changeset).where { EmReq.isMax(changeset) }
          else -> insert(changeset)
        })
        result?.let(json)?.also { status(201) } ?: ""
      }
    }

    http.patch("/queue/:id") { 
      EmReq(id = params("id").toInt())
        .let { Changeset.update(it, request.body().let(readJsonMap), ALLOWED_PARAMS) }
        .let { Repo.one(updateRecord(it)) }
        .let(json)
    }

    http.delete("/queue/:id") { 
      EmReq(id = params("id").toInt())
        .let { Repo.execute(deleteRecord(it)) }
    }

    http.delete("/queue") { 
      val comparingAgainst = {
        request.body()
          .takeUnless { it.isNullOrBlank() }
          ?.let(readJsonMap)
          ?.let { Changeset.newRecord<EmReq>(it, ALLOWED_PARAMS) }
      }

      Repo.execute(when (request.queryParamOrDefault("mode", "all")) {
        "greater" -> {
          val c = comparingAgainst() ?: run { status(422); return@delete "" }
          delete<EmReq>().where { d -> d["id"] `in` from<EmReq>().where { it greaterThan c }.select("id") }
        }
        "lesser" -> {
          val c = comparingAgainst() ?: run { status(422); return@delete "" }
          delete<EmReq>().where { d -> d["id"] `in` from<EmReq>().where { it lesserThan c }.select("id") }
        }
        "first" -> delete<EmReq>().where { d -> d["id"] `in` from<EmReq>().let(::descOrder).limit(1).select("id") }
        "last" -> delete<EmReq>().where { d -> d["id"] `in` from<EmReq>().let(::ascOrder).limit(1).select("id") }
        else -> delete()
      })
    }
  }
}

