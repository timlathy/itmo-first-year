package org.pearl.query

class ValueTransformer {
  companion object {
    fun escape(value: Any) = when (value) {
      is Query<*> -> '(' + value.toSql() + ')'
      is String -> value.replace("'", "\\'").let { '\'' + it + '\'' }
      else -> value.toString()
    }
  }
}
