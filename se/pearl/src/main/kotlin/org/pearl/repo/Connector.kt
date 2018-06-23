package org.pearl.repo

import java.sql.Connection
import java.sql.DriverManager
import java.sql.PreparedStatement
import java.sql.Statement

object Connector {
  var connection: Connection? = null

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
        if (param?.javaClass?.isEnum == true) setString(i + 1, param.toString())
        else setObject(i + 1, param)
      } }
      .use(block)

  inline fun <R> withStatement(block: (Statement) -> R): R =
    connection!!
      .createStatement()
      .use(block)
}
