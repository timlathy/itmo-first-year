package org.pearl.repo

import org.pearl.IdColumn
import org.pearl.Model
import kotlin.reflect.KClass
import kotlin.reflect.KProperty1
import kotlin.reflect.full.createInstance
import kotlin.reflect.full.declaredMemberProperties
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.jvm.javaType

class DDLWriter<T : Model>(val tableClass: KClass<T>) {
  fun tableDefinition(): String =
    """CREATE TABLE "${tableClass.createInstance().tableName}" (${propertiesToFields()})"""

  private fun propertiesToFields() =
    tableClass.declaredMemberProperties
      .map { escapeIdentifier(it.name) + " " + sqlType(it) + sqlModifiers(it) }
      .joinToString(", ")

  private fun escapeIdentifier(ident: String) = '"' + ident.replace("\"", "\\\"") + '"'

  private fun sqlModifiers(prop: KProperty1<T, *>) = when {
    prop.findAnnotation<IdColumn>() != null -> " PRIMARY KEY"
    !prop.returnType.isMarkedNullable -> " NOT NULL"
    else -> ""
  }

  private fun sqlType(prop: KProperty1<T, *>) =
    prop.returnType.javaType.typeName.let { typeName ->
      prop.findAnnotation<IdColumn>()?.run { when (typeName) {
        "int", "java.lang.Integer" -> "serial"
        "long", "java.lang.Long" -> "bigserial"
        else -> throw IllegalArgumentException("Unsupported type $typeName for a primary key in ${tableClass.simpleName}")
      } } ?: when (typeName) {
        "int", "java.lang.Integer" -> "integer"
        "long", "java.lang.Long" -> "bigint"
        "double", "java.lang.Double" -> "double precision"
        "java.lang.String" -> "text"
        "java.time.LocalDateTime" -> "timestamp"
        "java.time.ZonedDateTime" -> "timestampz"
        else -> throw IllegalArgumentException("Unsupported type $typeName for a column in ${tableClass.simpleName}")
      }
    }
}
