package ru.ifmo.se.lab2

import ru.ifmo.se.pokemon.*

val ALLIES = listOf(
  Moltres("Molly"), Koffing("Koffy"), Weezing("Weezy"),
  NidoranM("Niddy"), Nidorino("Niddo"), Nidoking("Kingy")
)

val FOES = listOf(
  Moltres("Molt"), Koffing("Koff"), Weezing("Weez"),
  NidoranM("Idan"), Nidorino("Rino"), Nidoking("Ning")
)

fun main(_args: Array<String>) {
  val battle = Battle()

  ALLIES.forEach { battle.addAlly(it) }
  FOES.forEach { battle.addFoe(it) }

  battle.go()
}
