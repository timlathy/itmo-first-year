package ru.ifmo.se.lab6.server

import java.net.ServerSocket
import java.util.concurrent.PriorityBlockingQueue
import kotlin.concurrent.thread

/* "The head of the queue is the least element with respect to the specified ordering",
 * which doesn't make much sense in our case, hence the flipped comparator. */
@JvmField val QUEUE_COMPARATOR: Comparator<EmploymentRequest> =
  Comparator.naturalOrder<EmploymentRequest>().reversed()

fun main(args: Array<String>) {
  val port = args.firstOrNull()?.toInt() ?: 8080
  println("Attempting to start the server bound to $port")

  val runner = CommandRunner(EmploymentRequestCommands.commandList,
    PriorityBlockingQueue(16, QUEUE_COMPARATOR))

  val server = ServerSocket(port)
  server.use {
    println("The server is now ready to accept connections.")
    while (true) {
      val client = it.accept()
      thread { RequestHandler(client, runner).run() }
    }
  }
}
