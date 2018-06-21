import org.pearl.IdColumn
import org.pearl.Model
import java.time.LocalDateTime

class
TestModel(
  @IdColumn val id: Int = 0,
  val name: String = "",
  val date: LocalDateTime = LocalDateTime.now(),
  val size: Int = 0,
  val enum: TestEnum = TestEnum.T1
): Model() {
  enum class TestEnum { T1, T2, T3 }
}
