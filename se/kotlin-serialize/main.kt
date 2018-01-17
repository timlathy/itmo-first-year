import kotlin.math.roundToInt

import kotlin.reflect.full.memberProperties
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.createInstance
import kotlin.reflect.KVisibility
import kotlin.reflect.KProperty1
import kotlin.reflect.KClass

import kotlin.annotation.Target
import kotlin.annotation.AnnotationTarget

@Target(AnnotationTarget.PROPERTY)
annotation class JsonIgnore

@Target(AnnotationTarget.PROPERTY)
annotation class JsonRename(val name: String)

interface JsonSerializer {
  fun serialize(obj: Any): String
}

@Target(AnnotationTarget.PROPERTY)
annotation class JsonSerializeWith(val serializer: KClass<out JsonSerializer>)

fun main(_args: Array<String>) {
  class RoundingDoubleSerializer: JsonSerializer {
    override fun serialize(value: Any) = (value as Double).roundToInt().toString()
  }

  data class InnerObject(
    val field: String)

  data class Person(
    val name: String,
    val age: Int,
    @JsonRename("happy") val satisfiedWithLife: Boolean,
    val innerObj: InnerObject = InnerObject("field value"),
    @JsonSerializeWith(RoundingDoubleSerializer::class) val height: Double = 173.4,
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
      val type = it.returnType
      val serialized = 
        if (value == null) "null"
        else it.findAnnotation<JsonSerializeWith>()?.serializer?.createInstance()?.serialize(value) ?:
             serializePrimitive(it, value) ?:
             serialize(value).toString()

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
