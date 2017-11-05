import com.github.michaelbull.result.*

interface Buyer {
  fun payCharge(amount: Int): Err<String>
}

open class Character(val name: String, funds: Int) : Buyer {
  var funds = funds
    private set

  override fun payCharge(amount: Int): Result<Valuable, String> {
    if (this.funds >= amount) {
      this.funds -= amount
      return Ok()
    }
    else {
      return Err("Oh, I don't have that much!")
    }
  }
}

