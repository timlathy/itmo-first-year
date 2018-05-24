package ru.ifmo.se.lab7.client.controllers

import ru.ifmo.se.lab7.client.ServerConnection
import ru.ifmo.se.lab7.client.models.EmploymentRequest
import tornadofx.*
import java.io.StringReader
import java.net.InetSocketAddress
import javax.json.Json

class EmploymentRequestController: Controller() {
  var objectList = SortedFilteredList<EmploymentRequest>(observableList())
    private set

  var connection = ServerConnection(InetSocketAddress(8080))

  enum class Actions(private val description: String) {
    ADD("add"),
    REMOVE("remove"),
    ADD_IF_HIGHEST_PRIORITY("add_if_max"),
    ADD_IF_LOWEST_PRIORITY("add_if_min"),
    REMOVE_ALL_HIGHER_PRIORITY("remove_greater"),
    REMOVE_ALL_LOWER_PRIORITY("remove_lower"),
    REMOVE_ALL("remove_all"),
    CHANGE_EXISTING("");

    override fun toString() = description
  }

  fun executeAction(action: Actions, element: EmploymentRequest, auxElement: EmploymentRequest?): String {
    val jsonElement = element.toJSON().toString()
    val jsonAuxElement = auxElement?.toJSON()?.toString()
    try {
      return when (action) {
        Actions.CHANGE_EXISTING -> {
          connection.fetchResponse("remove", jsonAuxElement!!)
          connection.fetchResponse("add", jsonElement)
        }
        else -> {
          connection.fetchResponse(action.toString(), jsonElement)
        }
      }
    }
    catch (e: ServerConnection.RequestFailureException) {
      return e.message
    }
  }

  fun setObjectListPredicate(pred: (EmploymentRequest) -> Boolean) {
    objectList.predicate = pred
  }

  fun refreshObjectList() {
    try {
      connection
        .fetchResponse("dump_queue")
        .let { Json.createReader(StringReader(it)) }
        .readArray()
        .toModel<EmploymentRequest>()
        .let { objectList.items.setAll(it) }
    }
    catch (e: ServerConnection.RequestFailureException) {
      /* Silently ignoring it for now...*/
      /* TODO: show an error in the UI */
    }
  }
}
