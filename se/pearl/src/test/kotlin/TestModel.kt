import Consts.defaultDate
import org.pearl.Id
import org.pearl.Model
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime

data class TestModel(
  @Id val id: Int = 0,
  val name: String = "",
  val date: LocalDateTime = defaultDate,
  val size: Int = 0,
  val enum: TestEnum = TestEnum.T1
): Model() {
  enum class TestEnum { T1, T2, T3 }
}

object Consts {
  val defaultDate = LocalDateTime.of(1970, 1, 1, 12, 0, 0)
  val defaultZonedDate = ZonedDateTime.of(defaultDate, ZoneId.systemDefault())
}
