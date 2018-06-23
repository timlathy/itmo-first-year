package org.pearl

import Consts.defaultDate
import Consts.defaultZonedDate
import kotlin.test.Test
import kotlin.test.assertEquals

import java.time.LocalDateTime
import java.time.ZonedDateTime
import kotlin.test.assertTrue

class ChangesetTest {
  data class ChangesetTestModel (
    @Id val id: Int = 0,
    val name: String = "",
    val double: Double = 1.1,
    val date: LocalDateTime = defaultDate,
    val zonedDate: ZonedDateTime = defaultZonedDate,
    val enum: SampleEnum = SampleEnum.VAL1
  ): Model() {
    enum class SampleEnum { VAL1, VAL2 }
  }

  @Test
  fun `should create changesets for new records`() {
    val expected = Changeset(ChangesetTestModel(),
      changes = mapOf(
        "name" to "h",
        "double" to -1.8,
        "date" to LocalDateTime.of(2017, 2, 3, 16, 25, 3),
        "zonedDate" to defaultZonedDate,
        "enum" to ChangesetTestModel.SampleEnum.VAL2
      ),
      errors = emptyList())

    assertEquals(expected, Changeset.newRecord(
      params = mapOf(
        "id" to "1",
        "name" to "h",
        "double" to "-1.8",
        "date" to "2017-02-03T16:25:03",
        "enum" to "VAL2",
        "random" to ""
      ),
      allowedParams = listOf("name", "double", "date", "enum")
    ))
  }

  @Test
  fun `should create changesets for record updates`() {
    val existingRecord = ChangesetTestModel(id = 1, name = "", double = 1.34)
    val changeset = Changeset.update(existingRecord, mapOf("name" to "hbc", "double" to "35"), listOf("name"))

    assertEquals(mapOf("name" to "hbc"), changeset.changes)
    assertEquals(emptyList(), changeset.errors)
  }

  @Test
  fun `should report casting errors`() {
    assertEquals(listOf("Incorrect value provided for \"enum\""),
      Changeset.newRecord<ChangesetTestModel>(params = mapOf(
        "enum" to "UNKNOWN_VAL"
      ), allowedParams = listOf("enum")).errors)

    assertEquals(listOf("Incorrect value provided for \"double\""),
      Changeset.newRecord<ChangesetTestModel>(params = mapOf(
        "double" to "1.22.3"
      ), allowedParams = listOf("double")).errors)
  }

  @Test
  fun `should support validation`() {
    val changeset = Changeset.newRecord<ChangesetTestModel>(emptyMap(), emptyList())
    assertTrue { changeset.errors.isEmpty() }

    assertEquals(
      listOf("Name is required"),
      changeset.validate<String>("name", { !it.isNullOrBlank() }, "Name is required").errors)

    assertTrue { changeset.errors.isEmpty() }
  }
}
