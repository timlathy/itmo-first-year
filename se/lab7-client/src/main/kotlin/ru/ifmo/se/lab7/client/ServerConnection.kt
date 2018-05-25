package ru.ifmo.se.lab7.client

import com.fasterxml.jackson.annotation.JsonRawValue
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import java.io.*
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

open class ServerConnection(private val serverAddr: SocketAddress) {
  data class Request(val action: String, @JsonRawValue val payload: String?)
  data class Response(val status: Int = 0, val data: Any = "")

  class RequestFailureException(override val message: String): Exception(message)

  private val mapper = ObjectMapper()

  open fun fetchResponse(action: String, rawPayload: String? = null): String =
    try {
      SocketChannel.open(serverAddr).use { channel ->
        val request = mapper.writeValueAsBytes(Request(action, rawPayload)) + '\n'.toByte()
        val buffer = ByteBuffer.allocate(request.size)

        buffer.clear()
        buffer.put(request)
        buffer.flip()
        while (buffer.hasRemaining()) channel.write(buffer)

        buffer.clear()
        StringBuilder().apply {
          while (channel.read(buffer) > 0) {
            buffer.rewind()
            val bufferContents = Charsets.UTF_8.decode(buffer)

            if (bufferContents.contains('\n')) {
              append(bufferContents.split('\n', limit = 2).first())
              append('\n')
              break
            }

            append(bufferContents)
            buffer.flip()
          }
          if (isEmpty() || last() != '\n') {
            throw RequestFailureException("Received invalid response from the server. " +
              "Please make sure the data you are entering is correct and retry your request.")
          }
        }.toString().let { response ->
          val (status, data) = mapper.readValue<Response>(response)
          if (status != 200) throw RequestFailureException(data.toString())

          /* Handling raw values requires a custom deserialization class */
          mapper.writeValueAsString(data)
        }
      }
    }
    catch (_: IOException) {
      throw RequestFailureException("Unable to establish connection with the server. " +
        "Please make sure the URI you have specified is correct: $serverAddr")
    }
}
