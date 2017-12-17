package ru.ifmo.se.lab3.domain

import com.fasterxml.jackson.annotation.JsonValue

enum class PersonSocialClass(private val description: String) {
  UNDERCLASS("Poor, likely homeless and unemployed"),
  POOR_WORKER("Lower class, may have an underpaid job without an opportunity for career advancement"),
  BLUE_COLLAR("Middle class, a skilled worker"),
  WHITE_COLLAR("Upper middle class, highly educated and having a high income"),
  WEALTHY("Upper class, having a lot of money either from owning a business or inheriting it");

  companion object {
    fun fromAccountBalance(balance: Int) =
      if (balance < 100) UNDERCLASS
      else if (balance < 500) POOR_WORKER
      else if (balance < 2500) BLUE_COLLAR
      else if (balance < 6000) WHITE_COLLAR
      else WEALTHY
  }

  @JsonValue
  override fun toString() = description
}
