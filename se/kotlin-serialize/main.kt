import kotlin.reflect.full.memberProperties
import kotlin.reflect.KVisibility
import kotlin.reflect.KProperty1

fun main(_args: Array<String>) {
  data class Person(
    val name: String,
    val age: Int,
    private val hidden: String = "hidden")
  
  val person = Person("Name", 20)
  println(serialize(person))
}

fun serialize(obj: Any): String =
  obj.javaClass.kotlin.memberProperties
    .filter {
      it.visibility == KVisibility.PUBLIC
    }
    .map {
      val value =
        it.get(obj)
      val serialized =
        if (value == null) "null"
        else serializePrimitive(it, value) ?: serialize(value).toString()

      encode(it.name, serialized)
    }
    .joinToString(prefix = "{", postfix = "}") 

fun encode(key: String, value: String): String =
  "\"${key.escape()}\": \"${value.escape()}\""

fun String.escape(): String =
  this.replace("\\", "\\\\").replace("\"", "\\\"")

val PRIMITIVE_TYPES = listOf("kotlin.Int", "kotlin.String")

fun <T, R> serializePrimitive(field: KProperty1<T, out R>, value: Any): String? =
  if (field.returnType.toString() in PRIMITIVE_TYPES) value.toString()
  else null
