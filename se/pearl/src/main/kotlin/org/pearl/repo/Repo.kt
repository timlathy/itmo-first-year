package org.pearl.repo

import org.pearl.Model
import org.pearl.query.SelectQuery
import org.pearl.reflection.enumByValue
import org.pearl.reflection.java
import org.pearl.reflection.javaName
import java.sql.*
import kotlin.reflect.KClass
import kotlin.reflect.full.primaryConstructor

typealias QualifiedClassName = String
typealias NameType = Pair<String, String>

const val ENUM_TAG = "-ENUM-"

object Repo {
  var connection: Connection? = null
  private val memoizedConstructorFields: MutableMap<QualifiedClassName, List<NameType>> = mutableMapOf()

  init {
    Class.forName("org.postgresql.Driver")
  }

  fun connectToUrl(host: String, port: Int, dbname: String, username: String, password: String) {
    connection = DriverManager.getConnection("jdbc:postgresql://$host:$port/$dbname", username, password)
  }

  inline fun <R> withPrepared(sqlWithParams: Pair<String, List<Any?>>, block: (PreparedStatement) -> R): R =
    connection!!
      .prepareStatement(sqlWithParams.first)
      .apply { sqlWithParams.second.forEachIndexed { i, param ->
        /* Enums are stored as strings in the database */
        if (param?.javaClass?.isEnum ?: false) setString(i + 1, param.toString())
        else setObject(i + 1, param)
      } }
      .use(block)

  inline fun <R> withStatement(block: (Statement) -> R): R =
    connection!!
      .createStatement()
      .use(block)

  fun updateWithResult(sqlWithParams: Pair<String, List<Any?>>) =
    withPrepared(sqlWithParams) { it.executeQuery() }

  inline fun <reified T : Model> fetchMany(query: SelectQuery<T>): List<T> =
    withPrepared(query.toParameterizedSql()) { statement ->
      val result = mutableListOf<T>()
      with(statement.executeQuery()) {
        while(next()) {
          T::class.primaryConstructor
            ?.call(*constructorParams(this, T::class))
            ?.let(result::add)
        }
      }
      result
    }

  fun rawSqlUpdate(sql: String) =
    withStatement { it.executeUpdate(sql) }

  inline fun <reified T : Model> createTable() =
    DDLWriter(T::class).tableDefinition().let { ddl -> withStatement { it.executeUpdate(ddl) } }

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
        ?: throw IllegalArgumentException("${modelClass.simpleName} does not have a primary constructor.")
    }
}
