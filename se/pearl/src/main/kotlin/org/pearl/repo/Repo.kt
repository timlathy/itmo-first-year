package org.pearl.repo

import org.pearl.Model
import org.pearl.query.Query
import java.sql.*

object Repo {
  var connection: Connection? = null

  init {
    Class.forName("org.postgresql.Driver")
  }

  fun connectToUrl(host: String, port: Int, dbname: String, username: String, password: String) {
    connection = DriverManager.getConnection("jdbc:postgresql://$host:$port/$dbname", username, password)
  }

  inline fun <R> withStatement(block: (Statement) -> R): R = connection!!.createStatement().use(block)

  inline fun <reified T : Model> fetchMany(query: Query<T>): List<T> =
    withStatement { statement ->
      val result = mutableListOf<T>()
      with(statement.executeQuery(query.toSql())) {
        while(next()) {
          T::class.constructors
        }
      }
      listOf()
    }

  inline fun <reified T : Model> createTable() =
    DDLWriter(T::class).tableDefinition().let { ddl -> withStatement { it.executeUpdate(ddl) } }
}
