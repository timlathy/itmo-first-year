package org.pearl.reflection

import kotlin.reflect.KAnnotatedElement
import kotlin.reflect.KType
import kotlin.reflect.jvm.javaType

fun enumByValue(enumClass: String, value: String) =
  Class
    .forName(enumClass)
    .getMethod("valueOf", String::class.java)
    .invoke(null, value)

fun KType.javaName() = this.javaType.typeName
fun KType.java() = (this.javaType as Class<*>)

inline fun <reified T : Annotation> KAnnotatedElement.hasAnnotation() =
  annotations.any { it is T }
