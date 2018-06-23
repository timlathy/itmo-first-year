package org.pearl

import org.pearl.repo.ParameterizedSql

object Sql {
  @JvmStatic
  fun tableDefinition(model: Model) =
    """CREATE TABLE "${model.tableName}" (${tableColumns(model)})"""

  @JvmStatic
  fun insertion(changeset: Changeset<*>): ParameterizedSql =
    Pair("""INSERT INTO "${changeset.record.tableName}" (${changeset.changes.keys.joinToString(", ", transform = ::identifier)}) """ +
      "VALUES (${Array(changeset.changes.size, { "?" }).joinToString(", ")}) RETURNING *", changeset.changes.values.toList())

  @JvmStatic
  private fun tableColumns(model: Model) =
    model.schema.entries.joinToString(", ") {
      identifier(it.key) + ' ' + it.value.sqlType +
        when {
          it.value.isPrimaryKey -> " PRIMARY KEY"
          !it.value.isNullable -> " NOT NULL"
          else -> ""
        }
    }

  @JvmStatic
  private fun identifier(unescapedName: String) = '"' + unescapedName.replace("\"", "\\\"") + '"'

}
