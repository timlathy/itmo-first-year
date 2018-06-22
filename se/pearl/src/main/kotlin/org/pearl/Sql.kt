package org.pearl

class Sql {
  companion object {
    fun escapeIdentifier(unescapedName: String) = unescapedName.replace("\"", "\\\"")

    fun tableDefinition(model: Model) =
      """CREATE TABLE "${model.tableName}" (${tableColumns(model)})"""

    private fun tableColumns(model: Model) =
      model.schema.entries.joinToString(", ") {
        '"' + escapeIdentifier(it.key) + '"' + ' ' + it.value.sqlType +
          when {
            it.value.isPrimaryKey -> " PRIMARY KEY"
            !it.value.isNullable -> " NOT NULL"
            else -> ""
          }
      }
  }
}
