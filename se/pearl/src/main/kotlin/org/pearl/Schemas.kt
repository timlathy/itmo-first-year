package org.pearl

import org.pearl.reflection.hasAnnotation
import org.pearl.reflection.java
import org.pearl.reflection.javaName
import kotlin.reflect.KClass
import kotlin.reflect.KProperty1
import kotlin.reflect.full.declaredMemberProperties

typealias Schema = Map<String, Schemas.Column>

object Schemas {
  data class Column(val sqlType: String, val isNullable: Boolean, val isPrimaryKey: Boolean)

  private val schemas = mutableMapOf<Class<out Model>, Schema>()

  fun <T : Model> ofModel(modelClass: Class<T>): Schema =
    schemas.getOrPut(modelClass, { buildSchema(modelClass) })

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
    prop.hasAnnotation<Id>() -> when (prop.returnType.javaName()) {
      "int", "java.lang.Integer" -> "serial"
      "long", "java.lang.Long" -> "bigserial"
      else -> throw IllegalArgumentException(
        "Unsupported type ${prop.returnType.javaName()} for a primary key in ${modelClass.simpleName}")
    }
    prop.returnType.java().isEnum -> "text"
    else -> when (prop.returnType.javaName()) {
      "int", "java.lang.Integer" -> "integer"
      "long", "java.lang.Long" -> "bigint"
      "double", "java.lang.Double" -> "double precision"
      "java.lang.String" -> "text"
      "java.time.LocalDateTime" -> "timestamp"
      "java.time.ZonedDateTime" -> "timestampz"
      else -> throw IllegalArgumentException(
        "Unsupported type ${prop.returnType.javaName()} for a column in ${modelClass.simpleName}")
    }
  }
}
