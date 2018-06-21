package org.pearl.query

import org.pearl.Model
import kotlin.reflect.full.createInstance

inline fun <reified T : Model> from() = Query(T::class.createInstance())

class Query<T : Model>(
  private val model: T,
  private val predicate: WherePredicate? = null,
  private val selectList: List<String>? = null,
  private val limit: Int? = null
) {
  fun select(vararg column: String) = Query(model, predicate, column.toList(), limit)

  /* See `WherePredicate.Column` for how predicates are built */
  fun where(expr: (T) -> WherePredicate) = Query(model, expr(model))

  fun limit(by: Int) = Query(model, predicate, selectList, by)

  fun toParameterizedSql(): Pair<String, List<Any>> {
    val table = model.tableName
    val projection = selectList?.map { "\"$table\".\"$it\"" }?.joinToString(", ") ?: "*"
    val selection = predicate?.let { " WHERE $it" } ?: ""
    val limit = limit?.let { " LIMIT $it" } ?: ""
    val bindings = predicate?.bindings ?: emptyList()

    return Pair("SELECT $projection FROM \"$table\"" + selection + limit, bindings)
  }
}

