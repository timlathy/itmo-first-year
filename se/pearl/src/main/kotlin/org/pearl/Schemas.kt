package org.pearl

import org.pearl.reflection.hasAnnotation
import org.pearl.reflection.java
import org.pearl.reflection.javaName
import kotlin.reflect.KClass
import kotlin.reflect.KProperty1
import kotlin.reflect.full.declaredMemberProperties
import org.pearl.Schemas.Column.SqlType.Primitive
import org.pearl.Schemas.Column.SqlType.EnumStringified
import org.pearl.Schemas.Column.SqlType.Primitive.PrimitiveMapping.*
import java.time.LocalDateTime
import java.time.ZonedDateTime

typealias Schema = Map<String, Schemas.Column>

object Schemas {
  data class Column(val sqlType: SqlType, val isNullable: Boolean, val isPrimaryKey: Boolean) {
    sealed class SqlType {
      data class Primitive(val mapping: PrimitiveMapping): SqlType() {
        enum class PrimitiveMapping(val sqlType: String, val cast: (String) -> Any) {
          INT_AUTOINCR("serial", String::toInt),
          LONG_AUTOINCR("bigserial", String::toLong),
          INT("integer", String::toInt),
          LONG("long", String::toLong),
          DOUBLE("double precision", String::toDouble),
          STRING("text", { it }),
          DATETIME("timestamp", LocalDateTime::parse),
          DATETIME_ZONED("timestampz", ZonedDateTime::parse)
        }

        override fun toString() = mapping.sqlType
      }
      data class EnumStringified(val enumConstants: Array<Enum<*>>): SqlType() {
        override fun toString() = "text"
      }
    }
  }

  private val schemas = mutableMapOf<Class<out Model>, Schema>()

  fun <T : Model> ofModel(modelClass: Class<T>): Schema =
    schemas.getOrPut(modelClass) { buildSchema(modelClass) }

  private fun <T : Model> buildSchema(modelClass: Class<T>) =
    modelClass.kotlin.declaredMemberProperties
      .map { it.name to columnFromProperty(it, modelClass.kotlin) }
      .toMap()

  private fun <T : Model> columnFromProperty(prop: KProperty1<T, *>, modelClass: KClass<T>)
    = Column(sqlType(prop, modelClass),
      isNullable = prop.returnType.isMarkedNullable,
      isPrimaryKey = prop.hasAnnotation<Id>()
    )

  private fun <T : Model> sqlType(prop: KProperty1<T, *>, modelClass: KClass<T>) = when {
    prop.hasAnnotation<Id>() ->
      when (prop.returnType.javaName()) {
        "int", "java.lang.Integer" -> Primitive(INT_AUTOINCR)
        "long", "java.lang.Long" -> Primitive(LONG_AUTOINCR)
        else -> throw IllegalArgumentException(
          "Unsupported type ${prop.returnType.javaName()} for a primary key in ${modelClass.simpleName}")
      }
    prop.returnType.java().isEnum ->
      EnumStringified(prop.returnType.java().getMethod("values").invoke(null) as Array<Enum<*>>)
    else ->
      when (prop.returnType.javaName()) {
        "int", "java.lang.Integer" -> Primitive(INT)
        "long", "java.lang.Long" -> Primitive(LONG)
        "double", "java.lang.Double" -> Primitive(DOUBLE)
        "java.lang.String" -> Primitive(STRING)
        "java.time.LocalDateTime" -> Primitive(DATETIME)
        "java.time.ZonedDateTime" -> Primitive(DATETIME_ZONED)
        else -> throw IllegalArgumentException(
          "Unsupported type ${prop.returnType.javaName()} for a column in ${modelClass.simpleName}")
      }
  }
}
