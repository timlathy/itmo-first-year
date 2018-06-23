package org.pearl.query

import org.pearl.Model
import kotlin.reflect.full.createInstance

inline fun <reified T : Model> from() = SelectQuery(T::class.createInstance())

class SelectQuery<T : Model>(
  private val model: T,
  private val predicate: WherePredicate? = null,
  private val selectList: List<String>? = null,
  private val limit: Int? = null
) {
  fun select(vararg column: String) = SelectQuery(model, predicate, column.toList(), limit)

  /* See `WherePredicate.Column` for how predicates are built */
  fun where(expr: (T) -> WherePredicate) = SelectQuery(model, expr(model))

  fun limit(by: Int) = SelectQuery(model, predicate, selectList, by)

  fun toParameterizedSql(): Pair<String, List<Any>> {
    val table = model.tableName
    val projection = selectList?.joinToString(", ") { "\"$table\".\"$it\"" } ?: "*"
    val selection = predicate?.let { " WHERE $it" } ?: ""
    val limit = limit?.let { " LIMIT $it" } ?: ""
    val bindings = predicate?.bindings ?: emptyList()

    return Pair("SELECT $projection FROM \"$table\"" + selection + limit, bindings)
  }
}
