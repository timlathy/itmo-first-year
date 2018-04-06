package ru.ifmo.se.lab6.server

import java.net.ServerSocket
import java.util.concurrent.PriorityBlockingQueue
import kotlin.concurrent.thread

fun main(args: Array<String>) {
  val port = args.firstOrNull()?.toInt() ?: 8080

  val runner = CommandRunner(EmploymentRequestCommands.commandList, PriorityBlockingQueue())

  val server = ServerSocket(port)
  server.use {
    while (true) {
      val client = it.accept()
      thread { RequestHandler(client, runner).run() }
    }
  }
}
