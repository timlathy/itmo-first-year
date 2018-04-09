package ru.ifmo.se.lab6.client

import ru.ifmo.se.lab6.client.CommandRunner.Companion.initRunnerWithConnection
import java.net.InetSocketAddress
import kotlin.system.exitProcess

fun main(args: Array<String>) {
  try {
    val conn = ServerConnection(InetSocketAddress(8080))
    val runner = initRunnerWithConnection(conn)
    Repl(runner).loop()
  }
  catch (e: ServerConnection.RequestFailureException) {
    println(e.message)
    exitProcess(1)
  }
}
