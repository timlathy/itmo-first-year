package org.pearl.repo

import org.pearl.Model
import org.pearl.query.SelectQuery
import java.sql.*
import kotlin.reflect.KClass
import kotlin.reflect.full.primaryConstructor
import kotlin.reflect.jvm.javaType

typealias QualifiedClassName = String
typealias NameType = Pair<String, String>

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
      .apply { sqlWithParams.second.forEachIndexed { i, param -> setObject(i + 1, param) } }
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
        "java.time.LocalDateTime" -> results.getTimestamp(name).toLocalDateTime()
        else -> results.getObject(name)
      }
    }.toTypedArray()

  fun <T : Model> constructorFields(modelClass: KClass<T>) =
    memoizedConstructorFields.getOrPut(modelClass.qualifiedName ?: "") {
      modelClass.primaryConstructor?.parameters?.map { Pair(it.name!!, it.type.javaType.typeName) }
        ?: throw IllegalArgumentException("${modelClass.simpleName} does not have a primary constructor.")
    }
}
