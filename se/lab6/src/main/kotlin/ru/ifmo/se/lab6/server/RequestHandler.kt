package ru.ifmo.se.lab6.server

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.InvalidFormatException
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import java.io.InputStream
import java.net.Socket
import javax.validation.Validation

data class Request<T>(val action: String = "", val payload: T? = null)
data class Response(val status: Int, val data: String)

class RequestHandler(private val socket: Socket,
                     private val runner: CommandRunner<EmploymentRequest>) : Runnable {
  private val mapper = ObjectMapper()
  private val deserializer = RequestDeserializer(mapper, EmploymentRequest::class.java)

  override fun run() =
    socket.use { mapper.writeValue(it.getOutputStream(), prepareResponse(it.getInputStream())) }

  private fun prepareResponse(requestStream: InputStream): Response {
    val request: Request<EmploymentRequest> = try {
      deserializer.readStream(requestStream)
    }
    catch (e: RequestDeserializer.DeserializationException) {
      return Response(422, e.message)
    }
    catch (e: JsonProcessingException) {
      return Response(400, "Unable to parse the request; please make sure the data you are sending is valid JSON")
    }

    return try {
      val responseData = runner.eval(request.action, request.payload)
      Response(200, responseData)
    }
    catch (e: CommandRunner.MissingArgumentException) {
      Response(422, "Command \"${request.action}\" requires an argument specified in the \"payload\" field")
    }
    catch (e: CommandRunner.UnknownCommandException) {
      Response(422, "Unknown command \"${request.action}\"")
    }
  }
}

class RequestDeserializer<T>(private val mapper: ObjectMapper, elementClass: Class<T>) {
  private val validator = Validation.buildDefaultValidatorFactory().validator
  private val requestType = mapper.typeFactory.constructParametricType(Request::class.java, elementClass)

  class DeserializationException(override val message: String) : Exception(message)

  fun readStream(requestStream: InputStream): Request<T> =
    try {
      mapper.readValue<Request<T>>(requestStream, requestType).apply {
        if (payload != null) {
          validator.validate(validator)
            .takeIf { it.isNotEmpty() }
            ?.map { it.message }
            ?.sorted()
            ?.joinToString(", ")
            ?.let { violations ->
              throw DeserializationException(
                "The employment request specified is invalid: $violations")
            }
        }
      }
    } catch (e: UnrecognizedPropertyException) {
      throw DeserializationException("Unknown property \"${e.propertyName}\", " +
        "expecting one of " + e.knownPropertyIds.joinToString(", ") { "\"$it\"" })
    } catch (e: InvalidFormatException) {
      if (e.targetType.isEnum) {
        val constants = e.targetType.enumConstants.joinToString(", ") { "\"$it\"" }
        throw DeserializationException("Unknown value \"${e.value}\" for " +
          "an enumerated property \"${e.path.last().fieldName}\", expecting one of $constants")
      } else throw e
    }
}
