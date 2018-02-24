package ru.ifmo.se.lab5

import java.io.File

fun main(args: Array<String>) {
  val queueLocation = System.getenv("QUEUE") ?:
    throw IllegalArgumentException("Please pass the queue file location via QUEUE environment variable")
  val storage = PriorityQueueStorage<EmploymentRequest>(
    EmploymentRequest::class.java, File(queueLocation))

  with(storage) {
    val queue = read()
    Repl(CommandRunner(EmploymentRequestCommands()), queue).loop()
    write(queue)
  }
}