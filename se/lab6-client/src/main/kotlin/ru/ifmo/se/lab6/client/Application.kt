package ru.ifmo.se.lab6.client

import ru.ifmo.se.lab6.client.CommandRunner.Companion.initRunnerWithConnection
import java.net.InetSocketAddress
import kotlin.system.exitProcess

fun main(args: Array<String>) {
  val port = args.firstOrNull()?.toInt() ?: 8080

  try {
    val conn = ServerConnection(InetSocketAddress(port))
    val runner = initRunnerWithConnection(conn, CustomCommands.list())
    Repl(runner).loop()
  }
  catch (e: ServerConnection.RequestFailureException) {
    println(e.message)
    exitProcess(1)
  }
}
