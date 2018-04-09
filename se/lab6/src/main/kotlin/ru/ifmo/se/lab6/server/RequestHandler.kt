package ru.ifmo.se.lab6.server

import com.fasterxml.jackson.annotation.JsonRawValue
import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.InvalidFormatException
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import java.io.*
import java.net.Socket
import javax.validation.Validation

data class Request<T>(val action: String = "", val payload: T? = null)
data class Response(val status: Int, @JsonRawValue val data: String)

class RequestHandler(private val socket: Socket,
                     private val runner: CommandRunner<EmploymentRequest>) : Runnable {
  private val mapper = ObjectMapper().apply { findAndRegisterModules() }
  private val deserializer = RequestDeserializer(mapper, EmploymentRequest::class.java)

  override fun run() =
    socket.use {
      val inStream = BufferedReader(InputStreamReader(it.getInputStream()))
      val outStream =  BufferedWriter(OutputStreamWriter(it.getOutputStream()))

      val request = inStream.readLine()
      val response = mapper.writeValueAsString(prepareResponse(request))

      outStream.write(response + '\n')
      outStream.flush()
    }

  private fun serialize(value: Any) = mapper.writeValueAsString(value)

  private fun prepareResponse(request: String): Response {
    val request: Request<EmploymentRequest> = try {
      deserializer.readRequest(request)
    }
    catch (e: RequestDeserializer.DeserializationException) {
      return Response(422, serialize(e.message))
    }
    catch (e: JsonProcessingException) {
      return Response(400, serialize(
        "Unable to parse the request; please make sure the data you are sending is valid JSON"))
    }

    return try {
      val responseData = runner.eval(request.action, request.payload)
      Response(200, mapper.writeValueAsString(responseData))
    }
    catch (e: CommandRunner.MissingArgumentException) {
      Response(422, serialize(
        "Command \"${request.action}\" requires an argument specified in the \"payload\" field"))
    }
    catch (e: CommandRunner.UnknownCommandException) {
      Response(422, serialize("Unknown command \"${request.action}\""))
    }
  }
}

class RequestDeserializer<T>(private val mapper: ObjectMapper, elementClass: Class<T>) {
  private val validator = Validation.buildDefaultValidatorFactory().validator
  private val requestType = mapper.typeFactory.constructParametricType(Request::class.java, elementClass)

  class DeserializationException(override val message: String) : Exception(message)

  fun readRequest(request: String): Request<T> =
    try {
      mapper.readValue<Request<T>>(request, requestType).apply {
        if (payload != null) {
          validator.validate(payload)
            .takeIf { it.isNotEmpty() }
            ?.map { it.message }
            ?.sorted()
            ?.joinToString(", ")
            ?.let { violations -> throw DeserializationException("Request payload is invalid: $violations") }
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
