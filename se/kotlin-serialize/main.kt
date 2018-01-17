import kotlin.reflect.full.memberProperties
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.KVisibility
import kotlin.reflect.KProperty1
import kotlin.reflect.KClass

import kotlin.annotation.Target
import kotlin.annotation.AnnotationTarget

@Target(AnnotationTarget.PROPERTY)
annotation class JsonIgnore

@Target(AnnotationTarget.PROPERTY)
annotation class JsonRename(val name: String)

fun main(_args: Array<String>) {
  data class InnerObject(
    val field: String)

  data class Person(
    val name: String,
    val age: Int,
    @JsonRename("happy") val satisfiedWithLife: Boolean,
    val innerObj: InnerObject = InnerObject("field value"),
    @JsonIgnore val ignored: String = "ignored",
    private val hidden: String = "hidden")
  
  val person = Person("Robert'); DROP TABLE Students;--", 16, true)
  println(serialize(person))
}

fun serialize(obj: Any): String =
  obj.javaClass.kotlin.memberProperties
    .filter {
      it.visibility == KVisibility.PUBLIC &&
        it.findAnnotation<JsonIgnore>() == null
    }
    .map {
      val name = it.findAnnotation<JsonRename>()?.name ?: it.name
      val value = it.get(obj)
      val serialized =
        if (value == null) "null"
        else serializePrimitive(it, value) ?: serialize(value).toString()

      encode(name, serialized)
    }
    .joinToString(prefix = "{", postfix = "}") 

fun encode(key: String, value: String): String =
  "\"${key.escape()}\": $value"

fun String.escape(): String =
  this.replace("\\", "\\\\").replace("\"", "\\\"")

fun <T, R> serializePrimitive(field: KProperty1<T, out R>, value: Any): String? =
  when (field.returnType.toString()) {
    "kotlin.Int" -> value.toString()
    "kotlin.String" -> "\"${(value as String).escape()}\""
    "kotlin.Boolean" -> value.toString()
    else -> null
  }
