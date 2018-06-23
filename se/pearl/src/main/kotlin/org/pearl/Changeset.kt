package org.pearl

import kotlin.reflect.full.createInstance
import org.pearl.Schemas.Column.SqlType.Primitive
import org.pearl.Schemas.Column.SqlType.Primitive.PrimitiveMapping.*
import org.pearl.Schemas.Column.SqlType.EnumStringified
import org.pearl.reflection.propertyValue

data class Changeset<T : Model>(val record: T, val changes: Map<String, Any?>, val errors: List<String>) {
  companion object {
    inline fun <reified T : Model> newRecord(params: Map<String, String>, allowedParams: List<String>): Changeset<T> =
      T::class.createInstance().let { defaultRecord ->
        val (changes, errors) = cast(defaultRecord.schema, { defaultRecord.propertyValue(it) }, params.filterKeys(allowedParams::contains))
        Changeset(defaultRecord, changes, errors)
      }

    fun cast(schema: Schema, defaultLookup: (String) -> Any?, params: Map<String, String>): Pair<Map<String, Any?>, List<String>> =
      schema.entries.fold(Pair(mutableMapOf(), mutableListOf())) { (changes, errors), (name, column) ->
        val param = params[name]
        when {
          param.isNullOrEmpty() && column.isNullable && column.sqlType != Primitive(STRING) ->
            Pair(changes + Pair(name, null), errors)
          param == null && column.isNullable ->
            Pair(changes + Pair(name, null), errors)
          param == null && !column.isPrimaryKey ->
            Pair(changes + Pair(name, defaultLookup(name)), errors)
          param != null && column.sqlType is EnumStringified ->
            column.sqlType.enumConstants
              .find { it.name == param }
              ?.let { Pair(changes + Pair(name, it), errors) }
              ?: Pair(changes, errors + "Incorrect value provided for \"$name\"")
          param != null && column.sqlType is Primitive ->
            try { Pair(changes + Pair(name, column.sqlType.mapping.cast(param)), errors) }
            catch (_: Exception) { Pair(changes, errors + "Incorrect value provided for \"$name\"") }
          else ->
            Pair(changes, errors)
        }
      }
  }

  inline fun <T> validate(column: String, validator: (T?) -> Boolean, message: String) =
    if (validator(changes[column] as T?)) this
    else Changeset(record, changes, errors + message)
}
