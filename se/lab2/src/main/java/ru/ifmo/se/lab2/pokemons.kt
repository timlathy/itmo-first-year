package ru.ifmo.se.lab2

import ru.ifmo.se.pokemon.*

open class Moltres(name: String, lvl: Int = 87) : Pokemon(name, lvl) { /* Swagger	requires lvl 87 */
  init {
    setStats(/* hp */ 90.0, /* att */ 100.0, /* def */ 90.0,
             /* spAtt */ 125.0, /* spDef */ 85.0, /* speed */ 90.0)
    setType(Type.FIRE, Type.FLYING)
    setMove(AncientPower(), Rest(), Overheat(), Swagger())
  }
}

open class Koffing(name: String, lvl: Int = 18) : Pokemon(name, lvl) { /* Sludge is lvl 18, the rest of the moves are learnt by TM? */
  init {
    setStats(40.0, 65.0, 95.0, 60.0, 45.0, 35.0)
    setType(Type.POISON)
    setMove(Swagger(), Venoshock(), Sludge())
  }
}

open class Weezing(name: String, lvl: Int = 18) : Koffing(name, lvl) {
  init {
    setStats(65.0, 90.0, 120.0, 85.0, 70.0, 60.0)
    setMove(Swagger(), Venoshock(), Sludge(), DoubleHit())
 }
}

open class NidoranM(name: String, lvl: Int = 1) : Pokemon(name, lvl) {
  init {
    setStats(46.0, 57.0, 40.0, 40.0, 40.0, 50.0)
    setType(Type.POISON)
    setMove(SmartStrike(), Blizzard())
  }
}

open class Nidorino(name: String, lvl: Int = 38) : NidoranM(name, lvl) {
  init {
    setStats(61.0, 72.0, 57.0, 55.0, 55.0, 65.0)
    setMove(SmartStrike(), Blizzard(), Flatter())
  }
}

open class Nidoking(name: String, lvl: Int = 1) : Nidorino(name, lvl) {
  init {
    setStats(81.0, 102.0, 77.0, 85.0, 75.0, 85.0)
    setMove(SmartStrike(), Blizzard(), Flatter(), Rest())
  }
}

