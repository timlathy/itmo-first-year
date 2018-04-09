package ru.ifmo.se.lab6.client

import com.fasterxml.jackson.annotation.JsonValue

data class TestSerializable (
  val someString: String = "",
  val someInt: Int = 0,
  val someEnum: TestEnum = TestEnum.NOT_SPECIFIED) {

  enum class TestEnum(private val description: String) {
    OPTION("Option"),
    ANOTHER_OPTION("Another option"),
    NOT_SPECIFIED("Not specified");

    @JsonValue
    override fun toString() = description
  }
}
