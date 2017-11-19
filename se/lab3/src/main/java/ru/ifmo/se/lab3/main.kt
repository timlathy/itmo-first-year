fun main(_args: Array<String>) {
  val c1 = Character("N", mapOf(Stat.BUDGET to 10, Stat.HUNGER to 70, Stat.APPEARANCE to 15))
}

class Character(val name: String, initStats: Map<Stat, Int>) {
  val stats = CharacterStats(initStats)

  override fun toString(): String = listOf(
      Stat.BUDGET,
      Stat.HUNGER,
      Stat.APPEARANCE
    ).map { stats.describeStat(it) }.joinToString(separator = " ")
}

enum class Stat {
  BUDGET {
    override fun describe(value: Int) = when (value) {
      in 0..49 -> "Very poor, hardly enough money for the basic needs."
      in 50..119 -> "Making ends meet."
      in 120..500 -> "Middle-class."
      else -> "Rich!"
    }
  },
  HUNGER {
    override fun describe(value: Int) = when (value) {
      in 70..100 -> "Exceptionally hungry, can't sleep or do anything but desperately look for something edible."
      in 30..69 -> "Has to eat pretty soon, lack of food is beginning to affect productivity."
      else -> "Feeling rather well."
    }
  },
  APPEARANCE {
    override fun describe(value: Int) = when (value) {
      in 0..29 -> "Appearance is not socially acceptable, highly unlikely to land a job, but pretty sure to lose one if still hired."
      in 30..39 -> "Unpleasant to be around, but may have a low-wage job with an irregular schedule."
      in 40..69 -> "Nothing off-putting about the appearance, may have a steady job with a decent income."
      else -> "Attractive and pleasant to be around, may have a very high-paying job."
    } 
  };

  abstract fun describe(value: Int): String
}

enum class Location(val description: String) {
  DOWNTOWN("City center, a rich commercial area"),
  SUBURBS("Living area for the middle class"),
  OUTSKIRTS("Area far away from the center, with cheap food and housing"),
}

class CharacterStats(initStats: Map<Stat, Int>) {
  private val _stats: MutableMap<Stat, Int> = initStats.toMutableMap()

  public fun getStat(type: Stat): Int? = _stats.get(type)
  
  public fun setStat(type: Stat, value: Int) = _stats.put(type, value)

  public fun describeStat(type: Stat): String? {
    val stat = _stats.get(type)
    return if (stat != null) type.describe(stat) else null
  }

  public fun satisfies(type: Stat, p: (Int) -> Boolean): Boolean {
    val stat = _stats.get(type)
    return (stat != null) && p(stat)
  }
}
