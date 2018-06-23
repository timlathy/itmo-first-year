package org.pearl.repo

import org.pearl.Changeset
import org.pearl.Model
import org.pearl.Sql
import org.pearl.query.SelectQuery
import org.pearl.reflection.enumByValue
import org.pearl.reflection.java
import org.pearl.reflection.javaName
import org.pearl.repo.Connector.withPrepared
import org.pearl.repo.Connector.withStatement
import java.sql.*
import kotlin.reflect.KClass
import kotlin.reflect.full.primaryConstructor

typealias QualifiedClassName = String
typealias NameTypeMap = Map<String, String>
typealias ParameterizedSql = Pair<String, List<Any?>>

const val ENUM_TAG = "-ENUM-"

object Repo {
  private val memoizedConstructorFields: MutableMap<QualifiedClassName, NameTypeMap> = mutableMapOf()

  fun connectToUrl(host: String, port: Int, dbname: String, username: String, password: String) =
    Connector.connectToUrl(host, port, dbname, username, password)

  inline fun <reified T : Model> fetchMany(query: SelectQuery<T>): List<T> =
    withPrepared(query.toParameterizedSql()) { instantiateMany(it.executeQuery()) }

  inline fun <reified T : Model> instantiateMany(results: ResultSet): List<T> =
    mutableListOf<T>().apply {
      while(results.next()) {
        T::class.primaryConstructor
          ?.call(*constructorParams(results, T::class))
          ?.let(::add)
      }
    }

  inline fun <reified T : Model> insert(changeset: Changeset<T>): T =
    withPrepared(Sql.insert(changeset)) { instantiateMany<T>(it.executeQuery()).first() }

  inline fun <reified T : Model> update(changeset: Changeset<T>): T =
    withPrepared(Sql.update(changeset)) { instantiateMany<T>(it.executeQuery()).first() }

  fun rawSqlUpdate(sql: String) =
    withStatement { it.executeUpdate(sql) }

  inline fun <reified T : Model> createTable() =
    Sql.tableDefinition(T::class.java.newInstance()).let { ddl -> withStatement { it.executeUpdate(ddl) } }

  fun <T : Model> constructorParams(results: ResultSet, modelClass: KClass<T>) =
    constructorFields(modelClass).map { (name, type) ->
      when(type) {
        /* Shortcuts for common types */
        "java.lang.String" -> results.getString(name)
        "int", "java.lang.Integer" -> results.getInt(name)
        "long", "java.lang.Long" -> results.getLong(name)
        "double", "java.lang.Double" -> results.getDouble(name)
        "boolean", "java.lang.Boolean" -> results.getBoolean(name)
        /* #getObject can't parse the timestamp returned by Postgres, so we do it by hand */
        "java.time.LocalDateTime" -> results.getTimestamp(name).toLocalDateTime()
        else ->
          if (type.startsWith(ENUM_TAG)) enumByValue(type.replaceFirst(ENUM_TAG, ""), results.getString(name))
          else results.getObject(name)
      }
    }.toTypedArray()

  fun <T : Model> constructorFields(modelClass: KClass<T>) =
    memoizedConstructorFields.getOrPut(modelClass.qualifiedName ?: "") {
      modelClass.primaryConstructor
        ?.parameters
        ?.map {
          /* Enums are handled separately -- see #constructorParams. */
          val type = if (it.type.java().isEnum) "$ENUM_TAG${it.type.javaName()}" else it.type.javaName()
          Pair(it.name!!, type)
        }
        ?.toMap()
        ?: throw IllegalArgumentException("${modelClass.simpleName} does not have a primary constructor.")
    }
}
