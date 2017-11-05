import com.github.michaelbull.result.*

open class Building

interface ResidableArea {
  fun acceptResident(b: Buyer, stay: Int): Err<String>
}

class Flophouse() : Building(), ResidableArea {
  private val COST_PER_NIGHT = 10
  private val NIGHT_DURATION = 12.0

  private val residents = HashMap<Buyer, Int>()

  override fun acceptResident(b: Buyer, stay: Int): Err<String> {
    val stayInNights = Math.ceil(stay.toDouble() / NIGHT_DURATION).toInt()
    val charge = stayInNights * COST_PER_NIGHT
    
    b.payCharge(charge)
     .andThen { residents.add(b, stayInNights); return Ok() }
  }
}

//open class Appartments(val residents: List<Character>) : Building()
//
//class BusinessHR() {
//  private val roles: HashMap<Character, BusinessRole>()
//
//  public fun getRole(e: Character): BusinessRole? = roles.get(e)
//  public fun addRole(e: Character, r: BusinessRole) = roles.put(e, r)
//  public fun removeRole(e: Character) = roles.remove(e)
//}
//
//open class Shop( val inventory: ShopInventory
//               , val branding: ShopBranding
//               , employees: List<Character> ) : Business(employees)
//
//open class ShopInventory()
//
//enum class ShopBranding(val displayName: String) {
//  PREMIUM("Premium"),
//  CASUAL("Casual"),
//  BUDGET("Budget")
//}
//
//class BusinessRole(val name: String, val wage: Int)
