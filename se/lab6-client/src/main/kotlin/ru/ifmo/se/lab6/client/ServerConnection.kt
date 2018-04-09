package ru.ifmo.se.lab6.client

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import java.io.*
import java.net.Socket
import java.net.SocketAddress

open class ServerConnection(private val serverAddr: SocketAddress) {
  data class Request(val action: String, val payload: Any?)
  data class Response(val status: Int = 0, val data: Any = "")

  class RequestFailureException(override val message: String): Exception(message)

  private val mapper = ObjectMapper()

  open fun fetchResponse(action: String, rawPayload: Any? = null): String =
    Socket().run {
      try {
        connect(serverAddr)
        val inStream = BufferedReader(InputStreamReader(getInputStream()))
        val outStream = BufferedWriter(OutputStreamWriter(getOutputStream()))

        val request = mapper.writeValueAsString(Request(action, rawPayload))

        outStream.write(request + "\n")
        outStream.flush()

        val response = inStream.readLine()
        val (status, data) = mapper.readValue<Response>(response)
        if (status != 200) throw RequestFailureException(data.toString())

        /* Handling raw values requires a custom deserialization class */
        mapper.writeValueAsString(data)
      }
      catch (_: IOException) {
        throw RequestFailureException("Unable to establish connection with the server. " +
          "Please make sure the URI you have specified is correct: $serverAddr")
      }
      finally {
        close()
      }
    }
}
