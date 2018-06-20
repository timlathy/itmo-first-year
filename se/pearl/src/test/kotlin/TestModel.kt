import org.pearl.Model
import java.time.LocalDateTime

class TestModel(
  val id: Int = 0,
  val name: String = "",
  val date: LocalDateTime = LocalDateTime.now(),
  val size: Int = 0
): Model()
