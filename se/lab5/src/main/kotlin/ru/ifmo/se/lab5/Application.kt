package ru.ifmo.se.lab5

import java.io.File

/* "The head of the queue is the least element with respect to the specified ordering",
 * which doesn't make much sense in our case, hence the flipped comparator. */
@JvmField val QUEUE_COMPARATOR: Comparator<EmploymentRequest> =
  Comparator.naturalOrder<EmploymentRequest>().reversed()

fun main(args: Array<String>) {
  val queueLocation = System.getenv("QUEUE") ?:
    throw IllegalArgumentException("Please pass the queue file location via QUEUE environment variable")
  val storage = PriorityQueueStorage(
    EmploymentRequest::class.java, File(queueLocation), QUEUE_COMPARATOR)

  with(storage) {
    val queue = read()
    Repl(CommandRunner(EmploymentRequestCommands(storage)), queue).loop()
    write(queue)
  }
}
