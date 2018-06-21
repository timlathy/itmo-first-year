import java.util.*

open class MapResourceBundle<K, V>(resources: Map<K, V>): ListResourceBundle() {
  private val contents = resources
    .entries
    .map { arrayOf(it.key as Any, it.value as Any) }
    .toTypedArray()

  override fun getContents() = contents
}
