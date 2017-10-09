package ru.ifmo.se.lab2

import java.util.Random
import ru.ifmo.se.pokemon.*

class AncientPower : SpecialMove(Type.ROCK, /* pow */ 60.0, /* acc */ 100.0) {
  override fun applySelfEffects(p: Pokemon) {
    p.addEffect(
        Effect().turns(0).chance(0.1)
          .stat(Stat.ATTACK, 1)
          .stat(Stat.DEFENSE, 1)
          .stat(Stat.SPECIAL_ATTACK, 1)
          .stat(Stat.SPECIAL_DEFENSE, 1)
          .stat(Stat.SPEED, 1))
  }

  override fun describe(): String = "AncientPower"
}

class Rest : StatusMove(Type.PSYCHIC, 0.0, 0.0) {
  override fun applySelfEffects(p: Pokemon) {
    p.addEffect(
        Effect().turns(2)) /* FIXME: sleep and restore HP? */
  }

  override fun describe(): String = "Rest"
}

class Overheat : SpecialMove(Type.FIRE, 130.0, 90.0) {
  override fun applySelfEffects(p: Pokemon) {
    p.addEffect(
        Effect().turns(0).stat(Stat.SPECIAL_ATTACK, -2))  
  }

  override fun describe(): String = "Overheat"
}

class Swagger : SpecialMove(Type.NORMAL, 0.0, 85.0) {
  override fun applyOppEffects(p: Pokemon) {
    Effect.confuse(p)
    p.addEffect(
        Effect().turns(0).stat(Stat.ATTACK, 2))
  }

  override fun describe(): String = "Swagger"
}

class Venoshock : SpecialMove(Type.POISON, 65.0, 100.0) {
  override fun applyOppEffects(p: Pokemon) {
    /* FIXME: inflict double damage if the target is poisoned? */
  }

  override fun describe(): String = "Venoshock"
}

class Sludge : SpecialMove(Type.POISON, 65.0, 100.0) {
  override fun applyOppEffects(p: Pokemon) {
    val targetImmune = p.hasType(Type.POISON) || p.hasType(Type.STEEL)
    val shouldPoison = Random().nextFloat() < 0.3 /* Effect.poison() is static, so chance() can't be used */

    if (!targetImmune && shouldPoison) Effect.poison(p)
  }

  override fun describe(): String = "Sludge"
}

class DoubleHit : PhysicalMove(Type.NORMAL, 35.0, 90.0) {
  /* I don't think it's possible to implement this,
   * you can't do _separate_ attacks from within a move AFAICS */

  override fun describe(): String = "DoubleHit"
}

/* TODO: is accuracy expressed in %? It should be set to max here */
class SmartStrike : PhysicalMove(Type.STEEL, 70.0, 100.0) {
  override fun describe(): String = "SmartStrike"
}

class Blizzard : SpecialMove(Type.ICE, 110.0, 70.0) {
  override fun applyOppEffects(p: Pokemon) {
    val targetImmune = p.hasType(Type.ICE)
    val shouldFreeze = Random().nextFloat() < 0.1 /* see Sludge */

    if (!targetImmune && shouldFreeze) Effect.freeze(p)
  }

  override fun describe(): String = "Blizzard"
}

class Flatter : StatusMove(Type.DARK, 0.0, 100.0) {
  override fun applyOppEffects(p: Pokemon) {
    Effect.confuse(p)
    p.addEffect(
        Effect().turns(0).stat(Stat.SPECIAL_ATTACK, 2))
  }

  override fun describe(): String = "Flatter"
}

