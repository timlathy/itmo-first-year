package org.pearl.reflection

import kotlin.reflect.KAnnotatedElement
import kotlin.reflect.KProperty1
import kotlin.reflect.KType
import kotlin.reflect.full.declaredMemberProperties
import kotlin.reflect.jvm.javaType

fun enumByValue(enumClass: String, value: String) =
  Class
    .forName(enumClass)
    .getMethod("valueOf", String::class.java)
    .invoke(null, value)

inline fun <reified T : Any> T.propertyValue(name: String): Any? =
  T::class.declaredMemberProperties.find { it.name == name }?.let {
    (it as KProperty1<T, Any?>).get(this)
  }

fun KType.javaName() = this.javaType.typeName
fun KType.java() = (this.javaType as Class<*>)

inline fun <reified T : Annotation> KAnnotatedElement.hasAnnotation() =
  annotations.any { it is T }
