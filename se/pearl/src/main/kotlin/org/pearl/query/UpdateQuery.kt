package org.pearl.query

import org.pearl.IdColumn
import org.pearl.Model
import org.pearl.repo.Repo
import kotlin.reflect.full.declaredMemberProperties
import kotlin.reflect.full.findAnnotation

fun <T : Model> insert(record: T) = UpdateQuery.InsertQuery(record).toParameterizedSql().let(Repo::updateWithResult)

sealed class UpdateQuery<T : Model> {
  data class InsertQuery<T : Model>(val record: T) : UpdateQuery<T>() {
    override fun toParameterizedSql() =
      record.javaClass.kotlin.declaredMemberProperties
        .filter { prop -> prop.annotations.none { it is IdColumn } }
        .map { Pair(it.name, it.get(record)) }
        .let {
          val idCol = record.javaClass.kotlin.declaredMemberProperties.find { it.findAnnotation<IdColumn>() != null }?.name
            ?: throw IllegalArgumentException("No ID column specified for ${record.tableName}")

          val cols = it.joinToString(", ") { "\"" + it.first + "\"" }
          val values = it.map { it.second }

          Pair("INSERT INTO \"${record.tableName}\" ($cols) VALUES (${it.joinToString(", ") { "?" }}) RETURNING \"$idCol\"", values)
        }
  }

  abstract fun toParameterizedSql(): Pair<String, List<Any?>>
}
