import org.pearl.IdColumn
import org.pearl.Model
import java.time.LocalDateTime

class
TestModel(
  @IdColumn val id: Int = 0,
  val name: String = "",
  val date: LocalDateTime = LocalDateTime.now(),
  val size: Int = 0
): Model()
