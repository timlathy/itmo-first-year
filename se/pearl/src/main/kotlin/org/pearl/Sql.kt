package org.pearl

import org.pearl.reflection.propertyValue
import org.pearl.repo.ParameterizedSql

object Sql {
  @JvmStatic
  fun tableDefinition(model: Model) =
    "CREATE TABLE ${ident(model.tableName)} (${tableColumns(model)})"

  @JvmStatic
  fun insert(changeset: Changeset<*>): ParameterizedSql =
    Pair("INSERT INTO ${ident(changeset.record.tableName)} " +
      "(${changeset.changes.keys.joinToString(", ", transform = ::ident)}) " +
      "VALUES (${Array(changeset.changes.size, { "?" }).joinToString(", ")}) RETURNING *", changeset.changes.values.toList())

  inline fun <reified T : Model> update(changeset: Changeset<T>) =
    changeset.record.schema.entries
      .find { (_, col) -> col.isPrimaryKey }
      ?.let { (key, _) -> Sql.update(Pair(key, changeset.record.propertyValue(key)!!), changeset) }
      ?: throw IllegalArgumentException("The model associated with the changeset has no primary key column.")

  @JvmStatic
  fun update(primaryKey: Pair<String, Any>, changeset: Changeset<*>): ParameterizedSql {
    val (keyColumn, keyValue) = primaryKey

    return Pair("UPDATE ${ident(changeset.record.tableName)} SET " +
      changeset.changes.keys.joinToString(", ") { "${ident(it)} = ?" } + ' ' +
      "WHERE ${ident(keyColumn)} = ? RETURNING *",
      changeset.changes.values + keyValue)
  }

  @JvmStatic
  private fun tableColumns(model: Model) =
    model.schema.entries.joinToString(", ") {
      ident(it.key) + ' ' + it.value.sqlType +
        when {
          it.value.isPrimaryKey -> " PRIMARY KEY"
          !it.value.isNullable -> " NOT NULL"
          else -> ""
        }
    }

  @JvmStatic
  private fun ident(unescapedName: String) = '"' + unescapedName.replace("\"", "\\\"") + '"'
}
