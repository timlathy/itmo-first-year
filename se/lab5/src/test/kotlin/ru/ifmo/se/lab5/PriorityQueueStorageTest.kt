package ru.ifmo.se.lab5

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File
import java.io.InputStream
import java.util.*

class PriorityQueueStorageTest {
  @Test
  fun `persists the queue in an external file`() {
    val comparator = kotlin.Comparator<TestSerializable> {
      o1, o2 -> o1.someInt - o2.someInt
    }
    val queue = PriorityQueue<TestSerializable>(comparator).apply {
      add(TestSerializable("h", 2, TestSerializable.TestEnum.OPTION))
      add(TestSerializable("b", 1, TestSerializable.TestEnum.NOT_SPECIFIED))
    }
    val file = File.createTempFile("storagetest", "tmp")

    var storage = PriorityQueueStorage<TestSerializable>(
      TestSerializable::class.java, file, comparator)

    assertTrue(storage.read().isEmpty())

    storage.write(queue)

    storage = PriorityQueueStorage<TestSerializable>(
      TestSerializable::class.java, file, comparator)

    assertArrayEquals(queue.toArray(), storage.read().toArray())

    file.delete()
  }
}
